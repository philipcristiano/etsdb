-module(etsdb_interval_fold).

-export([first_fold/1]).


first_fold(Interval) ->
    fun({K, V}, Acc) ->
        fold_bucket({K, V}, Acc, Interval)
    end.

fold_bucket({K, V}, [{PK, PV}| Acc], Bucket) ->
    NewBucket = K div Bucket * Bucket,
    if
        NewBucket == PK -> [{PK, PV}| Acc];
        NewBucket /= PK -> [{K div Bucket * Bucket, V}| [{PK, PV}| Acc]]
    end;
fold_bucket({K, V}, [], Bucket) ->
    [{K div Bucket * Bucket,V}].
