-module(integration_example).

-export([start/0]).

-define(MAX_LINE_LENGTH, 1000).

start() ->
    {ok, CurrentDir} = file:get_cwd(),
    ErlangExecutablePath = os:find_executable("erl"),
    BackendArgs = " -noshell -sname backend_node -s entry_point start",
    BackendDir = filename:join([CurrentDir, "backend_ebin"]),    
    %%BackendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, BackendDir}, stream, use_stdio, exit_status, stderr_to_stdout],
    BackendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, BackendDir}, stream, use_stdio],
    Backend = open_port({spawn, ErlangExecutablePath ++ BackendArgs}, BackendSettings),
    timer:sleep(20000),
    BackendPingResult = net_adm:ping('backend_node@polygon-vm'),
    io:format("BackendPingResult: ~p~n", [BackendPingResult]),
    CommandsInfo = gen_server:call({global_input_endpoint, 'backend_node@polygon-vm'}, {commands_info}),
    io:format("CommandsInfo: ~p~n", [CommandsInfo]),
    FrontendArgs = " -noshell -sname frontend_node -run cli_frontend_application main ../frontend_data/frontend.conf",
    FrontendDir = filename:join([CurrentDir, "frontend_ebin"]),
    FrontendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, FrontendDir}, stream, use_stdio],
    %%FrontendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, FrontendDir}, stream, use_stdio, exit_status, stderr_to_stdout],
    Frontend = open_port({spawn, ErlangExecutablePath ++ FrontendArgs}, FrontendSettings),
    timer:sleep(20000),
    FrontendPingResult = net_adm:ping('frontend_node@polygon-vm'),
    io:format("FrontendPingResult: ~p~n", [FrontendPingResult]),
    port_command(Frontend, "i?\n"),
    port_command(Frontend, "interface ?\n"),
    timer:sleep(60000),
    Messages = message_reader:read_all_messages(),
    io:format("Messages: ~p~n", [Messages]),
    port_close(Backend),
    port_close(Frontend).
