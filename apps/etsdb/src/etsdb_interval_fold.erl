-module(etsdb_interval_fold).

-export([first_fold/1]).
-export([online_fold/2]).


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

online_fold(Alg, Interval) ->
    case Alg of
        min -> {fun({K,V}, Acc) ->
                   fold_min({K, V}, Acc, Interval)
                end, []}
    end.

fold_min({eoi, eoi}, Acc, _Bucket) ->
    Acc;
fold_min({K, V}, [{PK, PV}| Acc], Bucket) ->
    NewBucket = K div Bucket * Bucket,
    if
        NewBucket == PK -> [{PK, erlang:min(PV, V)}| Acc];
        NewBucket /= PK -> [{K div Bucket * Bucket, V}| [{PK, PV}| Acc]]
    end;
fold_min({K, V}, [], Bucket) ->
    [{K div Bucket * Bucket,V}].
