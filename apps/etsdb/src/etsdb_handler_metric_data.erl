-module(etsdb_handler_metric_data).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    {Metric, Req1} = cowboy_req:binding(metric, Req),
    {TS1, Req1} = cowboy_req:binding(ts1, Req),
    {TS2, Req1} = cowboy_req:binding(ts2, Req),
    Data = etsdb:data(Metric, TS1, TS2),
    %Data = etsdb:data(<<"local.random.diceroll.2">>, <<"0">>,<<"2412434429">>),
	{ok, Req2} = cowboy_req:reply(200, [
		{<<"content-type">>, <<"application/json">>}
	], jsx:encode([Data]), Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
