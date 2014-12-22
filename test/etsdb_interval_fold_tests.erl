%%%-------------------------------------------------------------------
%%% @author $AUTHOR
%%% @copyright 2014 $OWNER
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

-module(etsdb_interval_fold_tests).
-define(MUT, etsdb_interval_fold).

interval_fold_test_() ->
    {foreach,
     spawn,
     fun start/0,
     fun stop/1,
     [fun first_fold/1,
      fun first_fold_float/1,
      fun same_bucket/1,
      fun multiple_buckets/1]}.

start() ->
    Ref = make_ref(),
    Ref.

stop(_Ref) ->
    ok.

first_fold(_Ref) ->
    F = ?MUT:first_fold(60),
    Acc = lists:foldl(F, [], [{1, 10}]),
    [?_assertEqual([{0, 10}], Acc)].

first_fold_float(_Ref) ->
    F = ?MUT:first_fold(60),
    Acc = lists:foldl(F, [], [{1, 10.0}]),
    [?_assertEqual([{0, 10.0}], Acc)].

same_bucket(_Ref) ->
    F = ?MUT:first_fold(60),
    Acc = lists:foldl(F, [], [{1, 10},
                              {2, 11}]),
    [?_assertEqual([{0, 10}], Acc)].

multiple_buckets(_Ref) ->
    F = ?MUT:first_fold(60),
    Acc = lists:foldl(F, [], [{1, 10},
                              {2, 11},
                              {61, 12},
                              {62, 13}]),
    [?_assertEqual([{60, 12}, {0, 10}], Acc)].

avg_first_test() ->
    {F, Acc} = etsdb_interval_fold:online_fold(<<"avg">>, 60),
    EndAcc = lists:foldl(F, Acc, [{0, 10.0}, {eoi, eoi}]),
    ?assertEqual([{0, 10.0}], EndAcc).

avg_two_items_test() ->
    {F, Acc} = etsdb_interval_fold:online_fold(<<"avg">>, 60),
    EndAcc = lists:foldl(F, Acc, [{0, 10.0},
                                  {1, 5.0},
                                  {eoi, eoi}]),
    ?assertEqual([{0, 7.5}], EndAcc).

avg_three_items_test() ->
    {F, Acc} = etsdb_interval_fold:online_fold(<<"avg">>, 60),
    EndAcc = lists:foldl(F, Acc, [{0, 10.0},
                                  {1, 5.0},
                                  {2, 45.0},
                                  {eoi, eoi}]),
    ?assertEqual([{0, 20.0}], EndAcc).

avg_four_items_test() ->
    {F, Acc} = etsdb_interval_fold:online_fold(<<"avg">>, 60),
    EndAcc = lists:foldl(F, Acc, [{0, 10.0},
                                  {1, 5.0},
                                  {2, 45.0},
                                  {3, 80.0},
                                  {eoi, eoi}]),
    ?assertEqual([{0, 35.0}], EndAcc).

avg_second_bucket_test() ->
    {F, Acc} = etsdb_interval_fold:online_fold(<<"avg">>, 60),
    EndAcc = lists:foldl(F, Acc, [{0, 10.0},
                                  {1, 5.0},
                                  {2, 45.0},
                                  {3, 80.0},
                                  {60, 1.0},
                                  {eoi, eoi}]),
    ?assertEqual([{60, 1.0}, {0, 35.0}], EndAcc).

avg_second_bucket_two_item_test() ->
    {F, Acc} = etsdb_interval_fold:online_fold(<<"avg">>, 60),
    EndAcc = lists:foldl(F, Acc, [{0, 10.0},
                                  {1, 5.0},
                                  {2, 45.0},
                                  {3, 80.0},
                                  {60, 1.0},
                                  {61, 3.0},
                                  {eoi, eoi}]),
    ?assertEqual([{60, 2.0}, {0, 35.0}], EndAcc).

avg_second_bucket_three_item_test() ->
    {F, Acc} = etsdb_interval_fold:online_fold(<<"avg">>, 60),
    EndAcc = lists:foldl(F, Acc, [{0, 10.0},
                                  {1, 5.0},
                                  {2, 45.0},
                                  {3, 80.0},
                                  {60, 1.0},
                                  {61, 3.0},
                                  {62, 5.0},
                                  {eoi, eoi}]),
    ?assertEqual([{60, 3.0}, {0, 35.0}], EndAcc).


min_first_test() ->
    {F, Acc} = etsdb_interval_fold:online_fold(<<"min">>, 60),
    EndAcc = lists:foldl(F, Acc, [{1, 10.0}, {eoi, eoi}]),
    ?assertEqual([{0, 10.0}], EndAcc).

min_two_items_test() ->
    {F, Acc} = etsdb_interval_fold:online_fold(<<"min">>, 60),
    EndAcc = lists:foldl(F, Acc, [{1, 10.0}, {2, 5.0}, {eoi, eoi}]),
    ?assertEqual([{0, 5.0}], EndAcc).

min_buckets_test() ->
    {F, Acc} = etsdb_interval_fold:online_fold(<<"min">>, 60),
    EndAcc = lists:foldl(F, Acc, [{1, 10.0},
                                  {2, 5.0},
                                  {60, 1.0},
                                  {61, 2.0},
                                  {eoi, eoi}]),
    ?assertEqual([{60, 1.0}, {0, 5.0}], EndAcc).
