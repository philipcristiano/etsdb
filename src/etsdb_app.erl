-module(etsdb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = etsdb_sup:start_link(),
    ok = riak_core:register([{vnode_module, etsdb_vnode}]),
    ok = riak_core_node_watcher:service_up(etsdb, self()),
    {ok, Pid}.

stop(_State) ->
    ok.
