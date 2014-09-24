-module(etsdb).
-export([write/2,
         run/0,
         open/1,
         write_to_db/3,
         list/1,
         get/2,
         fold_fun/2]).


run() ->
    {ok, Ref} = open("data"),
    eleveldb:fold(Ref, fun fold_fun/2, 0, []),
    {ok, Ref}.


open(Path) ->
    eleveldb:open(Path, [{create_if_missing, true}]).

write_to_db(Ref, Key, Value) ->
    eleveldb:write(Ref, [{put, Key, Value}], [{sync, true}]).

get(Ref, Key) ->
    eleveldb:get(Ref, Key, []).


fold_fun({Key, Value}, Acc) ->
    io:format("Found one! ~p => ~p ~n", [Key, Value]),
    Acc.

write(Key, Value) ->
    HashKey = chash:key_of(term_to_binary(Key)),

    %% Get the preflist...
    NVal = 1,
    [Pref] = riak_core_apl:get_apl(HashKey, NVal, etsdb),

    %% Send the play command...
    riak_core_vnode_master:sync_command(Pref, ping, etsdb_vnode_master),
    Message = {write, Key, Value},
    riak_core_vnode_master:sync_command(Pref, Message, etsdb_vnode_master),
    ok.

list(Key) ->
    HashKey = chash:key_of(term_to_binary(Key)),

    %% Get the preflist...
    NVal = application:get_env(riak_core, ring_size, 64),
    PrefList = riak_core_apl:get_apl(HashKey, NVal, etsdb),

    run_command(PrefList, list).

run_command([], _Command) ->
    ok;
run_command([Pref| List], Command) ->
    riak_core_vnode_master:sync_command(Pref, list, etsdb_vnode_master),
    run_command(List, Command).

