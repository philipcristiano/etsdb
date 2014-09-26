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
         handle_exit/3]).

-record(state, {partition, dbref}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, DBRef} = etsdb:open(string:concat("data/", erlang:integer_to_list(Partition))),
    {ok, #state{partition=Partition, dbref=DBRef}}.

% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    io:format("Got a ping!~n"),
    {reply, {pong, State#state.partition}, State};
handle_command({write, Key, Value}, _Sender, State=#state{dbref=DBRef}) ->
    etsdb:write_to_db(DBRef, Key, Value),
    {reply, {done, State#state.partition}, State};
handle_command({write, Metric, TS, Value}, _Sender, State=#state{dbref=DBRef}) ->
    Key = <<Metric/binary, <<":">>/binary, TS/binary>>,
    etsdb:write_to_db(DBRef, Key, Value),
    {reply, {done, State#state.partition}, State};
handle_command(list, _Sender, State=#state{dbref=DBRef})->
    eleveldb:fold(DBRef, fun etsdb:fold_fun/2, 0, []),
    {reply, {done, State#state.partition}, State};
handle_command(list_keys, _Sender, State=#state{dbref=DBRef})->
    Keys = eleveldb:fold_keys(DBRef, fun etsdb_vnode:all_keys/2, ordsets:new(), []),
    {reply, Keys, State};
handle_command({scan, Metric, TS1, TS2}, _Sender, State=#state{dbref=DBRef}) ->
    Key = <<Metric/binary, <<":">>/binary, TS1/binary>>,

    Acc =
        try
            eleveldb:fold(DBRef, etsdb_vnode:f_scan_until(TS2, fun list_callback/4), [], [{first_key, Key}])
        catch
            {done, Val} -> Val
        end,
    ForwardAcc = lists:reverse(Acc),
    {reply, {ok, ForwardAcc}, State};
handle_command(Message, _Sender, State) ->
    {noreply, State}.

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.


%% Internal API

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

list_callback(Metric, TS, Value, Acc) ->
    [{Metric, TS, Value} | Acc].

all_keys(Key, Set) ->
    Metric = case binary:split(Key, <<":">>, []) of
                [M, _TS] ->  M;
                [M] -> M;
                M -> M
    end,
    io:format("Add element ~p~n", [Metric]),
    ordsets:add_element(Metric, Set).
