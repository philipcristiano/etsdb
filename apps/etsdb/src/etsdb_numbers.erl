-module(etsdb_numbers).

-export([to_float/1]).

to_float(X) when is_list(X)->
    case string:to_float(X) of
        {error, no_float} -> erlang:float(erlang:list_to_integer(X));
        {Float, _Rest} -> Float
    end;
to_float(X) when is_binary(X)->
    to_float(erlang:binary_to_list(X)).
