-module (erlang_metrics_artificial_traffic).
-export([start_link/0]).

-behaviour(gen_statem).
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([handle_event/4]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, {}, []).

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> 
    handle_event_function.

init({}) ->
    process_flag(trap_exit, true),
    _ = rand:seed(exs1024s),
    Timeout = round(rand:uniform() * 449 + 50), % Random timeout between ticks ( 1 <-> 500 ms )
    {ok, initial_state, #{ pids => #{} }, [
        % {next_event, internal, start_random_tick}
        {timeout, Timeout, tick}
    ]}.

-spec handle_event('enter',
       OldState :: term(),
       State :: term(),
       Data :: term()) ->
        gen_statem:state_enter_result(term());
      (gen_statem:event_type(),
       Msg :: term(),
       State :: term(),
       Data :: term()) ->
        gen_statem:event_handler_result(term()).
handle_event({call, From}, Msg, State, Data) ->
    io:format("handle_event call Msg ~p ~p ~p\n", [Msg, State, Data]),
    {next_state, State, Data, [{reply,From,ok}]};
handle_event(cast, Msg, State, Data) ->
    io:format("handle_event cast Msg ~p ~p ~p\n", [Msg, State, Data]),
    {next_state, State, Data, []};
% handle_event(internal, start_random_tick, State, #{ pids := AlreadyPids } = Data) ->
handle_event(timeout, tick, State, #{ pids := AlreadyPids } = Data) ->
    % io:format("start_random_tick ~p ~p\n", [State, Data]),
    % io:format("tick ", []),
    Port = 54321,
    _ = rand:seed(exs1024s),
    ClientCount = round(rand:uniform() * 50), % Random number of clients ( 1 <-> 50 )
    Pids = lists:foldl(fun(Nmr, Acc) ->
        Acc#{ (spawn(fun() -> 
            % Client delay ( 1 <-> 200 ms )
            client(round(rand:uniform() * 199 + 1), Port) 
        end)) => Nmr }
    end, #{}, lists:seq(1, ClientCount)),
    Timeout = round(rand:uniform() * 449 + 50), % Random timeout between ticks ( 1 <-> 500 ms )
    % io:format("Timeout : ~p\n", [Timeout]),
    {keep_state, Data#{ pids => maps:merge(AlreadyPids,Pids) }, [
        {timeout, Timeout, tick}
    ]};
handle_event(info, Msg, State, Data) ->
    io:format("handle_event info Msg ~p ~p ~p\n", [Msg, State, Data]),
    'keep_state_and_data'.

terminate(_Reason, _StateName, _State) ->
    void.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

client(ClientTimeout, Port) ->
    % io:format("c.~p ", [ClientTimeout]),
    timer:sleep(ClientTimeout),
    {ok, ConnPid} = gun:open("localhost", Port),
    MRef = monitor(process, ConnPid),
    _StreamRef = gun:get(ConnPid, "/erlang_metrics_artificial_traffic"),
    receive
        {gun_response, ConnPid, StreamRef, fin, Status, Headers} ->
            no_data;
        {gun_response, ConnPid, StreamRef, nofin, Status, Headers} ->
            receive_data(ConnPid, MRef, StreamRef);
        {'DOWN', MRef, process, ConnPid, Reason} ->
            error_logger:error_msg("Oops!"),
            exit(Reason)
    after 
        60000 ->
            exit(timeout)
    end.

receive_data(ConnPid, MRef, StreamRef) ->
    receive
        {gun_data, ConnPid, StreamRef, nofin, Data} ->
            % io:format("~s~n", [Data]),
            receive_data(ConnPid, MRef, StreamRef);
        {gun_data, ConnPid, StreamRef, fin, Data} ->
            % io:format("~s~n", [Data]);
            ok;
        {'DOWN', MRef, process, ConnPid, Reason} ->
            error_logger:error_msg("Oops!"),
            exit(Reason)
    after 
        60000 ->
            exit(timeout)
    end.