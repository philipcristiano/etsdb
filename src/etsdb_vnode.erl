%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.

-module(etsdb_vnode).
-behaviour(riak_core_vnode).

-export([start_vnode/1,
         all_keys/2,
         init/1,
         fold_until/3,
         f_scan_until/2,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         request_hash/1,
         handle_exit/3]).

-record(state, {partition, dbref, datadir}).
-include_lib("riak_core/include/riak_core_vnode.hrl").

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    DataDir = application:get_env(etsdb, data_dir, "data/"),
    LDBDir = string:concat(DataDir, erlang:integer_to_list(Partition)),
    {ok, DBRef} = leveltsdb:open(LDBDir),
    {ok, #state{partition=Partition, dbref=DBRef, datadir=LDBDir}}.

handle_command(ping, _Sender, State) ->
    io:format("Got a ping!~n"),
    {reply, {pong, State#state.partition}, State};
handle_command({write, Metric, TS, Value}, _Sender, State=#state{dbref=DBRef}) ->
    leveltsdb:write(DBRef, Metric, TS, Value),
    {reply, {done, State#state.partition}, State};
handle_command(list, _Sender, State=#state{dbref=DBRef})->
    eleveldb:fold(DBRef, fun etsdb:fold_fun/2, 0, []),
    {reply, {done, State#state.partition}, State};
handle_command(list_keys, _Sender, State=#state{dbref=DBRef})->
    {ok, Keys} = leveltsdb:metrics(DBRef),
    io:format("Reply ~p~n", [Keys]),
    {reply, Keys, State};

handle_command({data, Metric, TS1, TS2}, _Sender, State) ->
    handle_command({data, Metric, TS1, TS2, []}, _Sender, State);
handle_command({data, Metric, TS1, TS2, Opts}, _Sender, State=#state{dbref=DBRef}) ->
    Alg = proplists:get_value(aggregation, Opts, <<"avg">>),
    {ok, Acc} = leveltsdb:aggregate(DBRef, Metric, TS1, TS2, Alg, Opts),
    {reply, {ok, Acc}, State};

handle_command({naive_repair, _Opts}, _Sender, State=#state{partition=Partition, dbref=DBRef}) ->
    io:format("Naive repair starting on ~p~n", [Partition]),
    {ok, Metrics} = leveltsdb:metrics(DBRef),
    io:format("Naive repair starting on ~p~n", [Partition]),
    repair_metrics(Metrics, DBRef),
    {reply, {done, Partition}, State};

handle_command(stop, _Sender, _State) ->
    {stop, normal, {}};

handle_command(_Message, _Sender, State) ->
    {noreply, State}.

handle_handoff_command(?FOLD_REQ{foldfun = Fun, acc0=Acc0},
                       _Sender, State=#state{dbref=DBRef}) ->
    io:format("FOLDREQ ~p~n", [State]),
    F = fun({Metric, TS ,Val}, Acc) -> Fun({Metric, TS}, Val, Acc) end,
    Acc = leveltsdb:fold_data(DBRef, F, Acc0),
    %% Acc = dict:fold(Fun, Acc0, State#state.store),
    {reply, Acc, State};
handle_handoff_command(_Message, _Sender, State) ->
    {forward, State}.

handoff_starting(_TargetNode, State) ->
    io:format("Handoff starting ~p~n", [_TargetNode]),
    {true, State}.

handoff_cancelled(State) ->
    io:format("Handoff cancelled ~p~n", [State]),
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    io:format("Handoff finished ~p~n", [State]),
    {ok, State}.

handle_handoff_data(_Data, State=#state{dbref=DBRef}) ->
    {{Metric, TS}, Value} = erlang:binary_to_term(_Data),
    leveltsdb:write(DBRef, Metric, TS, Value),
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    Data = erlang:term_to_binary({_ObjectName, _ObjectValue}),
    Data.

is_empty(State=#state{dbref=DBRef}) ->
    io:format("Checking is empty~n"),
    Empty = eleveldb:is_empty(DBRef),
    io:format("Checking is empty: ~p~n", [Empty]),
    {Empty, State}.

delete(State=#state{dbref=DBRef, datadir=DataDir}) ->
    io:format("Destroying!~n"),
    eleveldb:close(DBRef),
    eleveldb:destroy(DataDir, []),
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

request_hash(_) ->
    undefined.

%% Internal API

repair_metrics([], _Ref) ->
    ok;
repair_metrics([Metric| Rest], Ref) ->
    io:format("Repair metric: ~p~n", [Metric]),
    leveltsdb:fold_metric(Ref, Metric, rewrite_metric_aggregator(Metric), []),
    repair_metrics(Rest, Ref).

rewrite_metric_aggregator(Metric) ->
    fun({TS, Value}, Acc) ->
       ok = etsdb:write(Metric, TS, Value),
       Acc
    end.

fold_until(MetricName, EncodedEndTS, Callback) ->
    PrefixLength = size(MetricName),
    fun ({Key, Value}, Acc)->
        case Key of
            <<"m:", MetricName:PrefixLength/binary, ":", EncodedTS:32/integer>> ->
               case EncodedTS > EncodedEndTS of
                    true ->
                        throw({done, Acc});
                    false ->
                        Callback({EncodedTS, erlang:binary_to_term(Value)}, Acc)
               end;
            _ ->
                throw({done, Acc})
        end
    end.

f_scan_until(EndTS, Callback) ->
    fun ({Key, Value}, Acc)->
       [Metric, TS] = binary:split(Key, <<":">>, []),
       case TS > EndTS of
            true ->
                throw({done, Acc});
            false ->
                Callback(Metric, TS, Value, Acc)
       end
    end.

all_keys(Key, Acc) ->
    case Key of
        <<"k:", Metric/binary>> ->
            ordsets:add_element(Metric, Acc);
        _ ->
            throw({done, Acc})
    end.
