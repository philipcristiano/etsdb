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
      fun same_bucket/1,
      fun multiple_buckets/1]}.

start() ->
    Ref = make_ref(),
    Ref.

stop(_Ref) ->
    ok.

first_fold(_Ref) ->
    F = ?MUT:first_fold(60),
    Acc = F({<<"1">>, <<"10">>}, []),
    [?_assertEqual([{0, 10}], Acc)].

same_bucket(_Ref) ->
    F = ?MUT:first_fold(60),
    Acc = F({<<"1">>, <<"10">>}, []),
    Acc2 = F({<<"2">>, <<"11">>}, Acc),
    [?_assertEqual([{0, 10}], Acc2)].

multiple_buckets(_Ref) ->
    F = ?MUT:first_fold(60),
    Acc = F({<<"1">>, <<"10">>}, []),
    Acc2 = F({<<"2">>, <<"11">>}, Acc),
    Acc3 = F({<<"61">>, <<"12">>}, Acc2),
    Acc4 = F({<<"62">>, <<"13">>}, Acc3),
    [?_assertEqual([{60, 12}, {0, 10}], Acc4)].