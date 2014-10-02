-module(etsdb_interval_fold).

-export([first_fold/1]).


first_fold(Interval) ->
    fun({K, V}, Acc) ->
        [V|Acc]
    end.
