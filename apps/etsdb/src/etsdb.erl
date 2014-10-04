-module(etsdb).
-export([write/3,
         data/3,
         run/0,
         keys/0,
         open/1,
         write_to_db/3,
         list/1,
         get/2,
         scan/3,
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

write(Key, TS, Value) ->
    HashKey = chash:key_of(term_to_binary(Key)),

    %% Get the preflist...
    NVal = 3,
    PrefList = riak_core_apl:get_apl(HashKey, NVal, etsdb),

    Message = {write, Key, TS, Value},
    run_command(PrefList, ping),
    run_command(PrefList, Message),
    ok.

list(Key) ->
    HashKey = chash:key_of(term_to_binary(Key)),

    %% Get the preflist...
    NVal = application:get_env(riak_core, ring_size, 64),
    PrefList = riak_core_apl:get_apl(HashKey, NVal, etsdb),

    run_command(PrefList, list).

scan(Key, TS1, TS2) ->
    HashKey = chash:key_of(term_to_binary(Key)),
    NVal = 1,
    [Pref] = riak_core_apl:get_apl(HashKey, NVal, etsdb),

    Message = {scan, Key, TS1, TS2},
    riak_core_vnode_master:sync_command(Pref, Message, etsdb_vnode_master).

data(Key, Start, Stop) ->
    HashKey = chash:key_of(term_to_binary(Key)),
    NVal = 1,
    [Pref] = riak_core_apl:get_apl(HashKey, NVal, etsdb),

    Message = {data, Key, Start, Stop},
    riak_core_vnode_master:sync_command(Pref, Message, etsdb_vnode_master).


keys() ->
    HashKey = chash:key_of(<<"">>),
    NVal = application:get_env(riak_core, ring_size, 64),
    PrefList = riak_core_apl:get_apl(HashKey, NVal, etsdb),
    Message = list_keys,
    ordsets:to_list(ordsets:union(run_command(PrefList, Message))).



run_command([], _Command) ->
    [];
run_command([Pref| List], Command) ->
    io:format("Sending command to: ~p~n", [Pref]),
    R = riak_core_vnode_master:sync_command(Pref, Command, etsdb_vnode_master),
    [R | run_command(List, Command)].

