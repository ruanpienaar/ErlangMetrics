-module(erlang_metrics_handler).

-export([init/2]).

init(Req0, State) ->
    io:format("~p\n",[Req0]),
    ResponseCode = 200, % Just hardcode for now...
    % ok = prometheus_counter:inc(
    %     total_http_requests, [ResponseCode, cowboy_req:path(Req0)]
    % ),
    #{ path := Path,
       peer := {_PeerHost, PeerPort} 
    } = Req0,

    % prometheus_histogram:observe(response_time_in_microseconds,
    %                              [rawpath(Req)], Delta),

    Req1 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"Hello World!">>, Req0),
    {ok, Req1, State}.