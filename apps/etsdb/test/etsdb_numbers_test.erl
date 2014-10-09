-module(etsdb_numbers_test).

-include_lib("eunit/include/eunit.hrl").

to_float_test() ->
    ?assertEqual(4.0, etsdb_numbers:to_float("4.0")),
    ?assertEqual(4.0, etsdb_numbers:to_float(<<"4.0">>)),

    ?assertEqual(4.0, etsdb_numbers:to_float("4")),
    ?assertError(badarg, etsdb_numbers:to_float("X")).
