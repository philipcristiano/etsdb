-module(etsdb).
-export([write/2, run/0, open/1, write/3, get/2, fold_fun/2]).


run() ->
    {ok, Ref} = open("data"),
    eleveldb:fold(Ref, fun fold_fun/2, 0, []),
    {ok, Ref}.


open(Path) ->
    eleveldb:open(Path, [{create_if_missing, true}]).

write(Ref, Key, Value) ->
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
    Message = ping,
    riak_core_vnode_master:sync_command(Pref, ping, etsdb_vnode_master),
    ok.
