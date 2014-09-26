-module(etsdb).
-export([write/3,
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

write(Key, TS, Value) ->
    HashKey = chash:key_of(term_to_binary(Key)),

    %% Get the preflist...
    NVal = 3,
    PrefList = riak_core_apl:get_apl(HashKey, NVal, etsdb),

    %% Send the play command...
    Message = {write, Key, TS, Value},
    run_command(PrefList, Message),
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
    riak_core_vnode_master:sync_command(Pref, Command, etsdb_vnode_master),
    run_command(List, Command).

