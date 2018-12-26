-module(erlang_metrics_http).

-export([
    start_link/0,
    stop/0
]).

-include("erlang_metrics.hrl").

start_link() ->
    ok = metrics_startup(),
    {ok, X} = cowboy_startup(),

    io:format("!!!!!!!!!!! ~p \n\n\n\n", [{ok, X}]),

    {ok, X}.

metrics_startup() ->
    CounterFuns = [
        % fun() ->
        %     ok = prometheus_counter:new([
        %         {name, total_http_requests},
        %         {help, "Total HTTP requests"},
        %         {labels, [status, path]}
        %     ])
        % end,
        fun() ->
            ok = prometheus_histogram:new([
                {name, erlang_metrics_http_response_time},
                {labels, [path]},
                {buckets,
                    [10, 100, 1000, 10000, 100000, 300000, 500000,
                     750000, 1000000, 1500000, 2000000, 3000000]},
                {help,
                    "Microseconds between request start and response end."}
            ])
        end
    ],
    [ ok = try_create_counter_ignore_error(CounterFun) || CounterFun <- CounterFuns ],
    ok.

try_create_counter_ignore_error(F) ->
    try
        F()
    catch
        C:E ->
            io:format("~p ~p ~p ~p", 
                     [C, E, ?FUNCTION_NAME, ?MODULE]),
            ok
    end.

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
                cowboy_metrics_h,
                cowboy_compress_h,
                cowboy_stream_h
            ],
            metrics_callback => fun(CowboyMetricsMap) -> 
                forward_cowboy_metrics(CowboyMetricsMap)
            end
        }
    ).

-spec forward_cowboy_metrics(cowboy_metrics_h:metrics()) -> ok.
forward_cowboy_metrics(CowboyMetricsMap) ->
    % io:format("\n--------------------------------------\n\n"),
    % io:format("CowboyMetricsMap : ~p\n", [CowboyMetricsMap]),

    % RanchRef = maps:get(ref, CowboyMetricsMap, undefined),
    % ConnectionPid = maps:get(pid, CowboyMetricsMap, undefined),
    % CowboyStreamId = maps:get(streamid, CowboyMetricsMap, undefined),
    % CowboyStreamReason = maps:get(reason, CowboyMetricsMap, undefined),
    % CowboyReq = maps:get(req, CowboyMetricsMap, undefined),
    % CowboyPartialReq = maps:get(partial_req, CowboyMetricsMap, undefined),
    % ResponseStatus = maps:get(resp_status, CowboyMetricsMap, undefined),
    % Headers = maps:get(resp_headers, CowboyMetricsMap, undefined),
    ReqStart = maps:get(req_start, CowboyMetricsMap, undefined),
    %ReqEnd = maps:get(req_end, CowboyMetricsMap, undefined),
    % ReqBodyStart = maps:get(req_body_start, CowboyMetricsMap, undefined),
    % ReqBodyEnd = maps:get(req_body_end, CowboyMetricsMap, undefined),
    %RespStart = maps:get(resp_start, CowboyMetricsMap, undefined),
    RespEnd = maps:get(resp_end, CowboyMetricsMap, undefined),
    % EarlyErrorTime = maps:get(early_error_time, CowboyMetricsMap, undefined),
    % Procs = maps:get(procs, CowboyMetricsMap, undefined),
    % InformationalMetricsList = maps:get(informational, CowboyMetricsMap, undefined),
    % ReqBodyLength = maps:get(req_body_length, CowboyMetricsMap, undefined),
    % RespBodyLength = maps:get(resp_body_length, CowboyMetricsMap, undefined),

    Req = maps:get(req, CowboyMetricsMap),
    Path = cowboy_req:path(Req),

    % io:format("Metrics : ref ~p\n", [RanchRef]),
    % io:format("Metrics : pid ~p\n", [ConnectionPid]),
    % io:format("Metrics : streamid ~p\n", [CowboyStreamId]),
    % io:format("Metrics : reason ~p\n", [CowboyStreamReason]),
    % io:format("Metrics : req ~p\n", [CowboyReq]),
    % io:format("Metrics : partial_req ~p\n", [CowboyPartialReq]),
    % io:format("Metrics : resp_status ~p\n", [ResponseStatus]),
    % io:format("Metrics : resp_headers ~p\n", [Headers]),
%     io:format("Metrics : req_start ~p\n", [ReqStart]),
    % io:format("Metrics : req_end ~p\n", [ReqEnd]),
    % io:format("Metrics : req_body_start ~p\n", [ReqBodyStart]),
    % io:format("Metrics : req_body_end ~p\n", [ReqBodyEnd]),
    % io:format("Metrics : resp_start ~p\n", [RespStart]),
%     io:format("Metrics : resp_end ~p\n", [RespEnd]),
    % io:format("Metrics : early_error_time ~p\n", [EarlyErrorTime]),
    % io:format("Metrics : procs ~p\n", [Procs]),
    % io:format("Metrics : informational ~p\n", [InformationalMetricsList]),
    % io:format("Metrics : req_body_length ~p\n", [ReqBodyLength]),
    % io:format("Metrics : resp_body_length ~p\n", [RespBodyLength]),

    % io:format("\n\n--------------------------------------\n"),

    % (-576460741001991370 - -576460741031183225) / 1000000
    % RequestMiliSeconds = round((RespEnd - ReqStart) / 1000000),

    RespTime = (RespEnd - ReqStart),
    % io:format("RespTime ~p\n", [RespTime]),

    % io:format("Path ~p\n", [Path]),


    % case Path of
    %     <<"/erlang_metrics_artificial_traffic">> ->
    %         io:format("RequestMiliSeconds ~p\n", [RespTime/1000]);
    %     _ ->
    %         ok
    % end,

    prometheus_histogram:observe(
        % "_microseconds":
        % Prometheus.erl converts the Erlang native time difference to microseconds.
        erlang_metrics_http_response_time, [Path], RespTime / 1000000),

    ok.

stop() ->
    ok = ranch:stop_listener(?LISTENER_REF).

% --------------------------------------

% CowboyMetricsMap : #{informational => [],pid => <0.774.0>,
%                      procs =>
%                          #{<0.775.0> =>
%                                #{exit => -576460741001906535,reason => normal,
%                                  spawn => -576460741031172167}},
%                      reason => normal,ref => erlang_metrics_http,
%                      req =>
%                          #{body_length => 0,cert => undefined, 
%                            has_body => false,
%                            headers =>
%                                #{<<"accept">> => <<"*/*">>,
%                                  <<"host">> => <<"localhost:54321">>,
%                                  <<"user-agent">> => <<"curl/7.58.0">>},
%                            host => <<"localhost">>,method => <<"GET">>,
%                            path => <<"/metrics">>,
%                            peer => {{127,0,0,1},34862},
%                            pid => <0.774.0>,port => 54321,qs => <<>>,
%                            ref => erlang_metrics_http,scheme => <<"http">>,
%                            sock => {{127,0,0,1},54321},
%                            streamid => 1,version => 'HTTP/1.1'},
%                      req_body_end => undefined,req_body_length => 0,
%                      req_body_start => undefined,
%                      req_end => -576460741001901709,
%                      req_start => -576460741031183225,
%                      resp_body_length => 126147,
%                      resp_end => -576460741001991370,
%                      resp_headers =>
%                          #{<<"content-length">> => <<"126147">>,
%                            <<"content-type">> => <<"text/plain">>,
%                            <<"date">> => <<"Sat, 22 Dec 2018 22:52:33 GMT">>,
%                            <<"server">> => <<"Cowboy">>},
%                      resp_start => -576460741001991370,resp_status => 200,
%                      streamid => 1}
% Metrics : ref erlang_metrics_http
% Metrics : pid <0.774.0>
% Metrics : streamid 1
% Metrics : reason normal
% Metrics : req #{body_length => 0,cert => undefined,has_body => false,
%                 headers =>
%                     #{<<"accept">> => <<"*/*">>,
%                       <<"host">> => <<"localhost:54321">>,
%                       <<"user-agent">> => <<"curl/7.58.0">>},
%                 host => <<"localhost">>,method => <<"GET">>,
%                 path => <<"/metrics">>,
%                 peer => {{127,0,0,1},34862},
%                 pid => <0.774.0>,port => 54321,qs => <<>>,
%                 ref => erlang_metrics_http,scheme => <<"http">>,
%                 sock => {{127,0,0,1},54321},
%                 streamid => 1,version => 'HTTP/1.1'}
% Metrics : partial_req undefined
% Metrics : resp_status 200
% Metrics : resp_headers #{<<"content-length">> => <<"126147">>,
%                          <<"content-type">> => <<"text/plain">>,
%                          <<"date">> => <<"Sat, 22 Dec 2018 22:52:33 GMT">>,
%                          <<"server">> => <<"Cowboy">>}
% Metrics : req_start -576460741031183225
% Metrics : req_end -576460741001901709
% Metrics : req_body_start undefined
% Metrics : req_body_end undefined
% Metrics : resp_start -576460741001991370
% Metrics : resp_end -576460741001991370
% Metrics : early_error_time undefined
% Metrics : procs #{<0.775.0> =>
%                       #{exit => -576460741001906535,reason => normal,
%                         spawn => -576460741031172167}}
% Metrics : informational []
% Metrics : req_body_length 0
% Metrics : resp_body_length 126147


% --------------------------------------