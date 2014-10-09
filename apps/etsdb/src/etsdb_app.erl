-module(etsdb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = mkdirs(application:get_env(etsdb, data_dir, "data/")),
    {ok, Pid} = etsdb_sup:start_link(),
    ok = riak_core:register([{vnode_module, etsdb_vnode}]),
    ok = riak_core_node_watcher:service_up(etsdb, self()),
    GPPORT = application:get_env(etsdb, graphite_port, 2003),
    {ok, _} = ranch:start_listener(graphite_input, 1,
        ranch_tcp, [{port, GPPORT}], etsdb_graphite_protocol, []),
    start_cowboy(),
    {ok, Pid}.

stop(_State) ->
    ok.

mkdirs(Path) ->
    file:make_dir(Path),
    file:make_dir(Path ++ "/cluster_meta"),
    file:make_dir(Path ++ "/ring"),
    ok.

start_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", etsdb_handler_metrics, []},
               {"/static/[...]", cowboy_static, {dir, "priv/static/"}},
               {"/metrics", etsdb_handler_metrics, []},
               {"/metrics/:metric/", etsdb_handler_metric_data, []}
        ]}
    ]),
    CBHTTP = application:get_env(etsdb, http_port, 8080),
    cowboy:start_http(etsdb_http_listener, 100, [{port, CBHTTP}],
        [{env, [{dispatch, Dispatch}]}]
    ).
