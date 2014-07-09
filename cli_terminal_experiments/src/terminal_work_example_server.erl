-module(terminal_work_example_server).

-export([]).

-record(server_state, {socket = none, timeout = infinity, current_comand = none}).

start_server(Port, Timeout) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, 4}, {active, once}]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    server_event_loop(Socket, Timeout).

server_event_loop(Socket, Timeout) ->
    receive
        {tcp, Socket, Data} ->
            process_data(Data, Socket),
            inet:setopts(Socket, [{active, once}]),
            server_event_loop(Socket, Timeout);
        {tcp_closed, Socket} -> ok
    after Timeout -> ok
    end.

process_data(Data, Socket) -> ok.