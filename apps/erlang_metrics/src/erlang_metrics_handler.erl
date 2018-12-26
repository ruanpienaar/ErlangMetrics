-module(erlang_metrics_handler).

-export([init/2]).

init(Req0, State) ->

    % io:format("~p\n",[Req0]),
    % ResponseCode = 200, % Just hardcode for now...
    % ok = prometheus_counter:inc(
    %     total_http_requests, [ResponseCode, cowboy_req:path(Req0)]
    % ),

    % uncomment this, to see the histogram graph at 500 
    % timer:sleep(500),

    % in prometheus:
    % rate(erlang_metrics_http_response_time_sum{path="/erlang_metrics_artificial_traffic"}[1m]) 
    % / 
    % rate(erlang_metrics_http_response_time_count{path="/erlang_metrics_artificial_traffic"}[1m])

    

    #{ path := _Path,
       peer := {_PeerHost, PeerPort} 
    } = Req0,

    Req1 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"Hello World!">>, Req0),

    {ok, Req1, State}.