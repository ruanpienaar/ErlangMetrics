-module(erlang_metrics_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("erlang_metrics.hrl").

-define(LISTENER_REF, http).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = metrics_startup(),
    {ok, _} = cowboy_startup(),
    {ok, _} = erlang_metrics_sup:start_link().

stop(_State) ->
    ok = ranch:stop_listener(?LISTENER_REF).

%% ===================================================================
%% Internals
%% ===================================================================

metrics_startup() ->
    CounterFuns = [
        % fun() ->
        %     ok = prometheus_counter:new([
        %         {name, total_http_requests},
        %         {help, "Total HTTP requests"},
        %         {labels, [status, path]}
        %     ])
        % end,
        % fun() ->
        %     ok = prometheus_histogram:new([
        %         {name, response_time_in_microseconds},
        %         {labels, [path]},
        %         {buckets,
        %             [10, 100, 1000, 10000, 100000, 300000, 500000,
        %              750000, 1000000, 1500000, 2000000, 3000000]},
        %         {help,
        %             "Microseconds between request receipt and response send."}
        %     ])
        % end
    ],
    [ ok = try_create_counter_ignore_error(CounterFun) || CounterFun <- CounterFuns ],
    ok.

cowboy_startup() ->
    Routes = application:get_env(erlang_metrics, cowboy_routes, []),
    io:format("Cowboy routes to compile\n~p\n", [Routes]),
    Dispatch = cowboy_router:compile(Routes),
    Port = application:get_env(erlang_metrics, erlang_metrics_http_port, 54321),
    io:format("Cowboy socket port\n~p\n", [Port]),
    {ok, _} = cowboy:start_clear(
        ?LISTENER_REF, 
        [{port, Port}], 
        #{
            env => #{
                dispatch => Dispatch
            },
            stream_handlers => [
                cowboy_metrics_h
            ],
            metrics_callback => fun(CowboyMetricsMap) -> 
                forward_cowboy_metrics(CowboyMetricsMap)
            end
        }
    ).

try_create_counter_ignore_error(F) ->
    try
        F()
    catch
        C:E ->
            io:format("~p ~p ~p ~p", 
                     [C, E, ?FUNCTION_NAME, ?MODULE]),
            ok
    end.

-spec forward_cowboy_metrics(cowboy_metrics_h:metrics()) -> ok.
forward_cowboy_metrics(CowboyMetricsMap) ->
    io:format("CowboyMetricsMap : ~p\n", [CowboyMetricsMap]),

    RanchRef = maps:get(ref, CowboyMetricsMap, undefined),
    ConnectionPid = maps:get(pid, CowboyMetricsMap, undefined),
    CowboyStreamId = maps:get(streamid, CowboyMetricsMap, undefined),
    CowboyStreamReason = maps:get(reason, CowboyMetricsMap, undefined),
    CowboyReq = maps:get(req, CowboyMetricsMap, undefined),
    CowboyPartialReq = maps:get(partial_req, CowboyMetricsMap, undefined),
    ResponseStatus = maps:get(resp_status, CowboyMetricsMap, undefined),
    Headers = maps:get(resp_headers, CowboyMetricsMap, undefined),
    ReqStart = maps:get(req_start, CowboyMetricsMap, undefined),
    ReqEnd = maps:get(req_end, CowboyMetricsMap, undefined),
    ReqBodyStart = maps:get(req_body_start, CowboyMetricsMap, undefined),
    ReqBodyEnd = maps:get(req_body_end, CowboyMetricsMap, undefined),
    RespStart = maps:get(resp_start, CowboyMetricsMap, undefined),
    RespEnd = maps:get(resp_end, CowboyMetricsMap, undefined),
    EarlyErrorTime = maps:get(early_error_time, CowboyMetricsMap, undefined),
    Procs = maps:get(procs, CowboyMetricsMap, undefined),
    InformationalMetricsList = maps:get(informational, CowboyMetricsMap, undefined),
    ReqBodyLength = maps:get(req_body_length, CowboyMetricsMap, undefined),
    RespBodyLength = maps:get(resp_body_length, CowboyMetricsMap, undefined),

    io:format("Metrics : ref ~p\n", [RanchRef]),
    io:format("Metrics : pid ~p\n", [ConnectionPid]),
    io:format("Metrics : streamid ~p\n", [CowboyStreamId]),
    io:format("Metrics : reason ~p\n", [CowboyStreamReason]),
    io:format("Metrics : req ~p\n", [CowboyReq]),
    io:format("Metrics : partial_req ~p\n", [CowboyPartialReq]),
    io:format("Metrics : resp_status ~p\n", [ResponseStatus]),
    io:format("Metrics : resp_headers ~p\n", [Headers]),
    io:format("Metrics : req_start ~p\n", [ReqStart]),
    io:format("Metrics : req_end ~p\n", [ReqEnd]),
    io:format("Metrics : req_body_start ~p\n", [ReqBodyStart]),
    io:format("Metrics : req_body_end ~p\n", [ReqBodyEnd]),
    io:format("Metrics : resp_start ~p\n", [RespStart]),
    io:format("Metrics : resp_end ~p\n", [RespEnd]),
    io:format("Metrics : early_error_time ~p\n", [EarlyErrorTime]),
    io:format("Metrics : procs ~p\n", [Procs]),
    io:format("Metrics : informational ~p\n", [InformationalMetricsList]),
    io:format("Metrics : req_body_length ~p\n", [ReqBodyLength]),
    io:format("Metrics : resp_body_length ~p\n", [RespBodyLength]),

    ok.