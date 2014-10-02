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
     [fun first_fold/1]}.

start() ->
    Ref = make_ref(),
    Ref.

stop(_Ref) ->
    ok.

first_fold(_Ref) ->
    F = ?MUT:first_fold(60),
    Acc = F({<<"1">>, 10}, []),
    [?_assertEqual([10], Acc)].
