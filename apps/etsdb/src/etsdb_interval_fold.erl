-module(etsdb_interval_fold).

-export([first_fold/1]).


first_fold(Interval) ->
    fun({K, V}, Acc) ->
        fold_bucket({binary_to_integer(K), binary_to_integer(V)}, Acc, Interval)
    end.

fold_bucket({K, V}, [{PK, PV}| Acc], Bucket) ->
    NewBucket = K div Bucket,
    if
        NewBucket == PK -> [{PK, PV}| Acc];
        NewBucket /= PK -> [{K,V}| [{PK, PV}| Acc]]
    end;
fold_bucket({K, V}, [], Bucket) ->
    [{K div Bucket,V}].
