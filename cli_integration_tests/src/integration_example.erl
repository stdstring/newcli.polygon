-module(integration_example).

-export([start/0]).

-define(MAX_LINE_LENGTH, 1000).

start() ->
    {ok, CurrentDir} = file:get_cwd(),
    ErlangExecutablePath = os:find_executable("erl"),
    BackendArgs = " -noshell -sname backend_node -s entry_point start",
    BackendDir = filename:join([CurrentDir, "backend_ebin"]),    
    %%BackendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, BackendDir}, stream, use_stdio, exit_status, stderr_to_stdout],
    BackendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, BackendDir}, stream, use_stdio, exit_status, stderr_to_stdout],
    Backend = open_port({spawn, ErlangExecutablePath ++ BackendArgs}, BackendSettings),
    timer:sleep(20000),
    cli_backend_life_manager:start('backend_node@polygon-vm'),
    BackendPingResult = net_adm:ping('backend_node@polygon-vm'),
    io:format("BackendPingResult: ~p~n", [BackendPingResult]),
    io:format("RPC check result: ~p~n", [rpc:call('backend_node666@polygon-vm', erlang, whereis, [global_input_endpoint])]),
    FrontendArgs = " -noshell -sname frontend_node -run cli_frontend_application main ../frontend_data/frontend.conf -s init stop",
    FrontendDir = filename:join([CurrentDir, "frontend_ebin"]),
    FrontendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, FrontendDir}, stream, use_stdio, exit_status, stderr_to_stdout],
    %%FrontendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, FrontendDir}, stream, use_stdio, exit_status, stderr_to_stdout],
    Frontend = open_port({spawn, ErlangExecutablePath ++ FrontendArgs}, FrontendSettings),
    timer:sleep(20000),
    %%true = wait_node('frontend_node@polygon-vm', 10, 2000),
    FrontendPingResult = net_adm:ping('frontend_node@polygon-vm'),
    io:format("FrontendPingResult: ~p~n", [FrontendPingResult]),
    port_command(Frontend, "i?\n"),
    port_command(Frontend, "interface ?\n"),
    %%{Frontend,{data,{eol,"@CliDemo>CommandLine: \"i?\\n\""}}},
    true = wait_message(Frontend, "@CliDemo>CommandLine: \"i?\\n\"", 20000),
    %%{Frontend,{data,{eol,"interface\tinterface range"}}},
    true = wait_message(Frontend, "interface\tinterface range", 20000),
    %%{Frontend,{data,{eol,"@CliDemo>CommandLine: \"interface ?\\n\""}}},
    true = wait_message(Frontend, "@CliDemo>CommandLine: \"interface ?\\n\"", 20000),
    %%{Frontend,{data,{eol,"interface {interface-id} command"}}}
    true = wait_message(Frontend, "interface {interface-id} command", 20000),
    port_close(Frontend),
    port_close(Backend),
    cli_backend_life_manager:stop('backend_node@polygon-vm').

wait_message(SourcePort, MessageBody, Timeout) ->
    receive
        {SourcePort, {data, {eol, MessageBody}}} ->
            io:format("Receive message: ~p~n", [MessageBody]),
            true
    after Timeout -> false
    end.

%%wait_node(Node, 0, _WaitTime) ->
%%    net_adm:ping(Node) == pong;
%%wait_node(Node, Count, WaitTime) ->
%%    case net_adm:ping(Node) of
%%        pong -> true;
%%        pang ->
%%            timer:sleep(WaitTime),
%%            wait_node(Node, Count-1, WaitTime)
%%    end.