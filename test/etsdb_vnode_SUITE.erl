-module(etsdb_vnode_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([all/0]).
-export([groups/0, init_per_testcase/2, end_per_testcase/2]).
-export([list_keys/1,
         read/1,
         aggregate/1]).


send_command(Pid, Request) ->
    Ref = make_ref(),
    gen_fsm:send_event(Pid, ?VNODE_REQ{request=Request,
                                       sender={raw, Ref, self()}}),
    {ok, Ref}.

all() -> [{group, vnode}].

groups() -> [{vnode,
             [],
             [list_keys,
              read,
              aggregate]}].

get_response(Ref) ->
    receive {Ref, M} -> {ok, M}
    after 100 -> error
    end.


init_per_testcase(_, Config) ->
    Dir = ?config(priv_dir, Config),
    application:set_env(etsdb, data_dir, Dir),
    {ok, Pid} = riak_core_vnode:start_link(etsdb_vnode, 0, []),
    [{vnode, Pid}| Config].

end_per_testcase(_, Config) ->
    Pid = ?config(vnode, Config),
    send_command(Pid, stop),
    ok.

list_keys(Config) ->
    {Key, TS, Value} = {<<"Key">>, 1, 2},
    Pid = ?config(vnode, Config),

    {ok, _} = send_command(Pid, {write, Key, TS, Value}),
    {ok, Ref}  = send_command(Pid, list_keys),
    {ok, Msg} = get_response(Ref),

    ?assertEqual([Key], Msg).

read(Config) ->
    {Key, TS, Value} = {<<"Key">>, 1, 2},
    Pid = ?config(vnode, Config),

    {ok, _} = send_command(Pid, {write, Key, TS, Value}),
    {ok, Ref}  = send_command(Pid, {data, Key, 0, 2}),
    {ok, {ok, Msg}} = get_response(Ref),

    ?assertEqual([{0,2}], Msg).

aggregate(Config) ->
    {Key, TS} = {<<"Key">>, 1},
    Pid = ?config(vnode, Config),

    {ok, _} = send_command(Pid, {write, Key, TS, 5}),
    {ok, _} = send_command(Pid, {write, Key, TS + 1, 15}),
    {ok, Ref}  = send_command(Pid, {data, Key, 0, 2}),
    {ok, {ok, Msg}} = get_response(Ref),

    ?assertEqual([{0,10.0}], Msg).
