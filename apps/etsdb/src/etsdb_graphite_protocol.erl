-module(etsdb_graphite_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
	ok = ranch:accept_ack(Ref),
	loop(Socket, Transport, <<"">>).

loop(Socket, Transport, Buffer) ->
	case Transport:recv(Socket, 0, 1024) of
		{ok, Data} ->
            io:format("Got some data: ~p~n", [Data]),
            {ok, Unprocessed} = handle_data(Buffer, Data),
			loop(Socket, Transport, Unprocessed);
		_ ->
			ok = Transport:close(Socket)
	end.

handle_data(Buffer, NewData) ->
    Data = <<Buffer/binary, NewData/binary>>,
    Unprocessed = parse_data(Data),
    {ok, Unprocessed}.

parse_data(Data) ->
    case binary:split(Data, <<"\n">>) of
    [_] ->
        Data;
    [Line, Rest] ->
        write_message(Line),
        parse_data(Rest)
    end.

write_message(Messages) ->
    [Metric, Value, TS] = binary:split(Messages, [<<" ">>], [global]),
    io:format("Got record: ~p~p~p~n", [Metric, Value, TS]),
    etsdb:write(Metric, TS, Value),
    ok.
