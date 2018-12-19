-module(erlang_metrics_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("erlang_metrics.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/metrics", erlang_metrics_prometheus, []},
            {"/", erlang_metrics_handler, []}
        ]}
    ]),
    Port = application:get_env(erlang_metrics, erlang_metrics_http_port, 54321),
    {ok, _} = cowboy:start_clear(http, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }),
    erlang_metrics_sup:start_link().

stop(_State) ->
    ok.
