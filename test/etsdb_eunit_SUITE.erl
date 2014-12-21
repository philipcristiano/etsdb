-module(etsdb_eunit_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([groups/0]).
-export([interval/1,
         numbers/1]).

all() -> [{group, db}].

groups() -> [{db,
             [],
             [interval,
              numbers]}].

interval(_Config) ->
    ok = eunit:test(etsdb_interval_fold_tests).

numbers(_Config) ->
    ok = eunit:test(etsdb_numbers_test).
