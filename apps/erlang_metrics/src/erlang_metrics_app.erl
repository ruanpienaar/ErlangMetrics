-module(erlang_metrics_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("erlang_metrics.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, _} = erlang_metrics_sup:start_link().

stop(_State) ->
    erlang_metrics_http:stop().
