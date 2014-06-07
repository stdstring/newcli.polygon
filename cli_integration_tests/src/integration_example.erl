-module(integration_example).

-export([start/0]).

-define(MAX_LINE_LENGTH, 1000).
-define(BACKEND_NODE, 'backend_node@polygon-vm').
-define(FRONTEND_NODE, 'frontend_node@polygon-vm').

start() ->    
    {ok, CurrentDir} = file:get_cwd(),
    ErlangExecutablePath = os:find_executable("erl"),
    BackendArgs = prepare_args(" -noshell -sname ~s -s entry_point start", ?BACKEND_NODE),
    BackendDir = filename:join([CurrentDir, "backend_ebin"]),    
    BackendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, BackendDir}, stream, use_stdio, exit_status, stderr_to_stdout],
    Backend = open_port({spawn, ErlangExecutablePath ++ BackendArgs}, BackendSettings),
    true = wait_process(?BACKEND_NODE, global_input_endpoint, 10, 2000),
    cli_backend_life_manager:start(?BACKEND_NODE),
    FrontendArgs = prepare_args(" -noshell -sname ~s -run cli_frontend_application main ../frontend_data/frontend.conf -s init stop", ?FRONTEND_NODE),
    FrontendDir = filename:join([CurrentDir, "frontend_ebin"]),
    FrontendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, FrontendDir}, stream, use_stdio, exit_status, stderr_to_stdout],
    Frontend = open_port({spawn, ErlangExecutablePath ++ FrontendArgs}, FrontendSettings),
    true = wait_node(?FRONTEND_NODE, 10, 2000),
    lib:flush_receive(),
    port_command(Frontend, "i?\n"),
    port_command(Frontend, "interface ?\n"),
    %% {Frontend,{data,{eol,"@CliDemo>interface\tinterface range"}}},
    true = wait_message(Frontend, "@CliDemo>interface\tinterface range", 20000),
    %% {Frontend,{data,{eol,"@CliDemo>interface {interface-id} command"}}}
    true = wait_message(Frontend, "@CliDemo>interface {interface-id} command", 20000),
    [] = message_reader:read_all_messages(),
    port_close(Frontend),
    port_close(Backend),
    cli_backend_life_manager:stop(?BACKEND_NODE).

prepare_args(FormatArgsStr, Node) ->
    lists:flatten(io_lib:format(FormatArgsStr, [atom_to_list(Node)])).

wait_message(SourcePort, MessageBody, Timeout) ->
    receive
        {SourcePort, {data, {eol, MessageBody}}} ->
            io:format("Receive message: ~p~n", [MessageBody]),
            true
    after Timeout -> false
    end.

wait_node(Node, 0, _WaitTime) ->
    net_adm:ping(Node) == pong;
wait_node(Node, Count, WaitTime) ->
    case net_adm:ping(Node) of
        pong -> true;
        pang ->
            timer:sleep(WaitTime),
            wait_node(Node, Count-1, WaitTime)
    end.

wait_process(Node, ProcessName, 0, _WaitTime) ->
    case rpc:call(Node, erlang, whereis, [ProcessName]) of
        {badrpc,nodedown} -> false;
        unfefined -> false;
        _Pid -> true
    end;
wait_process(Node, ProcessName, Count, WaitTime) ->
    case rpc:call(Node, erlang, whereis, [ProcessName]) of
        {badrpc,nodedown} ->
            timer:sleep(WaitTime),
            wait_process(Node, ProcessName, Count-1, WaitTime);
        unfefined ->
            timer:sleep(WaitTime),
            wait_process(Node, ProcessName, Count-1, WaitTime);
        _Pid -> true
    end.