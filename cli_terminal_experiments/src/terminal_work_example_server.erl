-module(terminal_work_example_server).

-export([start/0]).

-record(server_state, {socket = none, timeout = infinity, command = none}).

start() -> start_server(22222, 60*1000).

start_server(Port, Timeout) ->
    process_flag(trap_exit, true),
    io:format("starting ... ~n", []),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, 4}, {active, once}]),
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
            server_event_loop(State);
        {'EXIT', Command, killed} ->
            tcp_gen:send(Socket, term_to_binary('end')),
            UpdatedState = State#server_state{command = none},
            server_event_loop(UpdatedState);
        {'EXIT', Command, _Reason} ->
            tcp_gen:send(Socket, term_to_binary("command execution error\n")),
            tcp_gen:send(Socket, term_to_binary('end')),
            UpdatedState = State#server_state{command = none},
            server_event_loop(UpdatedState);
        {response, command_finish} ->
            tcp_gen:send(Socket, term_to_binary('end')),
            UpdatedState = State#server_state{command = none},
            server_event_loop(UpdatedState);
        {response, command_continue, Data} ->
            tcp_gen:send(Socket, term_to_binary({result, Data})),
            server_event_loop(State);
        {tcp, Socket, Data} ->
            Request = binary_to_term(Data),
            UpdatedState = process_request(Request, State),
            inet:setopts(Socket, [{active, once}]),
            server_event_loop(UpdatedState);
        {tcp_closed, Socket} ->
            io:format("socket closed~n", []),
            ok
    after Timeout ->
        tcp_gen:send(Socket, term_to_binary(timeout)),
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
    tcp_gen:send(Socket, term_to_binary({result, "unknown command\n"})),
    tcp_gen:send(Socket, term_to_binary('end')),
    State.