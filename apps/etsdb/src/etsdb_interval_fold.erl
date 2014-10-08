-module(etsdb_interval_fold).

-export([first_fold/1]).
-export([online_fold/2]).

-record(avg_acc, {bucket, n, val, rest}).

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
        avg -> {fun({K,V}, Acc) ->
                   fold_avg({K, V}, Acc, Interval)
                end, #avg_acc{}};
        min -> {fun({K,V}, Acc) ->
                   fold_min({K, V}, Acc, Interval)
                end, []}
    end.

fold_avg({eoi, eoi}, #avg_acc{bucket=undefined, rest=Rest}, _Bucket) ->
    Rest;
fold_avg({eoi, eoi}, #avg_acc{bucket=CurBucket, val=Val,rest=undefined}, _Bucket) ->
    [{CurBucket, Val}];
fold_avg({eoi, eoi}, #avg_acc{bucket=CurBucket, val=Val,rest=Rest}, _Bucket) ->
    [{CurBucket, Val}| Rest];
fold_avg({K, V}, #avg_acc{bucket=undefined, n=undefined, val=undefined, rest=undefined}, Bucket) ->
    NewBucket = K div Bucket * Bucket,
    #avg_acc{bucket=NewBucket, n=1, val=V, rest=[]}.

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
