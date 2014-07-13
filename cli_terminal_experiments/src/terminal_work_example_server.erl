-module(terminal_work_example_server).

-export([start/0]).

-record(server_state, {socket = none, timeout = infinity, command = none}).

-define(END, {'end', ""}).
-define(TIMEOUT, {timeout, ""}).

start() -> start_server(22222, 2*60*1000).

start_server(Port, Timeout) ->
    process_flag(trap_exit, true),
    io:format("starting ... ~n", []),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, 4}, {active, once}]),
    %% {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, raw}, {active, once}]),
    io:format("listening ... ~n", []),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    State = #server_state{socket = Socket, timeout = Timeout},
    io:format("started ... ~n", []),
    server_event_loop(State).

server_event_loop(State) ->
    Socket = State#server_state.socket,
    Timeout = State#server_state.timeout,
    Command = State#server_state.command,
    receive
        {'EXIT', _Command, normal} ->
            io:format("command's normal exit~n", []),
            server_event_loop(State);
        {'EXIT', Command, killed} ->
            io:format("command's termination~n", []),
            gen_tcp:send(Socket, term_to_binary(?END)),
            UpdatedState = State#server_state{command = none},
            server_event_loop(UpdatedState);
        {'EXIT', Command, Reason} ->
            io:format("command's exit by reason: ~p~n", [Reason]),
            gen_tcp:send(Socket, term_to_binary({result, "command execution error\n"})),
            gen_tcp:send(Socket, term_to_binary(?END)),
            UpdatedState = State#server_state{command = none},
            server_event_loop(UpdatedState);
        {response, command_finish} ->
            io:format("command's finish~n", []),
            gen_tcp:send(Socket, term_to_binary(?END)),
            UpdatedState = State#server_state{command = none},
            server_event_loop(UpdatedState);
        {response, command_continue, Data} ->
            io:format("command's data: ~p~n", [Data]),
            gen_tcp:send(Socket, term_to_binary({result, Data})),
            server_event_loop(State);
        {tcp, Socket, Data} ->
            io:format("data from socket: ~p~n", [Data]),
            Request = binary_to_term(Data),
            UpdatedState = process_request(Request, State),
            inet:setopts(Socket, [{active, once}]),
            server_event_loop(UpdatedState);
        {tcp_closed, Socket} ->
            io:format("socket closed~n", []),
            ok
    after Timeout ->
        gen_tcp:send(Socket, term_to_binary(?TIMEOUT)),
        gen_tcp:close(Socket),
        ok
    end.

process_request("heavycmd", State) ->
    Command = terminal_work_example_heavy_command:start(),
    State#server_state{command = Command};
process_request("lightcmd", State) ->
    Command = terminal_work_example_light_command:start(),
    State#server_state{command = Command};
process_request("stop", #server_state{command = none} = State) ->
    State;
process_request("stop", #server_state{command = Command} = State) ->
    exit(Command, kill),
    State#server_state{command = none};
process_request(_Data, #server_state{socket = Socket} = State) ->
    gen_tcp:send(Socket, term_to_binary({result, "unknown command\n"})),
    gen_tcp:send(Socket, term_to_binary(?END)),
    State.