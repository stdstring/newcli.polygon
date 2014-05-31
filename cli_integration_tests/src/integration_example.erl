-module(integration_example).

-export([start/0]).

-define(MAX_LINE_LENGTH, 1000).

start() ->
    ErlangExecutablePath = os:find_executable("erl"),
    BackendArgs = " -noshell -sname backend_node -s entry_point start",
    %%BackendArgs = " -noshell -sname backend_node -s entry_point start > /tmp/b 2>&1",
    BackendDir = "/home/std-string/work/newcli/cli_integration_tests/backend_ebin/",
    %%BackendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, BackendDir}, stream, use_stdio, exit_status, stderr_to_stdout],
    BackendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, BackendDir}, stream, use_stdio, stderr_to_stdout],
    Backend = open_port({spawn, ErlangExecutablePath ++ BackendArgs}, BackendSettings),
    timer:sleep(20000),
    BackendPingResult = net_adm:ping('backend_node@polygon-vm'),
    io:format("BackendPingResult: ~p~n", [BackendPingResult]),
    CommandsInfo = gen_server:call({global_input_endpoint, 'backend_node@polygon-vm'}, {commands_info}),
    io:format("CommandsInfo: ~p~n", [CommandsInfo]),
    FrontendArgs = " -noshell -sname frontend_node -run cli_frontend_application main /home/std-string/work/newcli/cli_integration_tests/frontend_data/frontend.conf",
    %%FrontendArgs = " -noshell -sname frontend_node -run cli_frontend_application main /home/std-string/work/newcli/cli_integration_tests/frontend_data/frontend.conf > /tmp/f 2>&1",
    %%FrontendArgs = " -noshell -sname frontend_node -run cli_frontend_application main /home/std-string/work/newcli/cli_integration_tests/frontend_data/frontend.conf > /tmp/f",
    FrontendDir = "/home/std-string/work/newcli/cli_integration_tests/frontend_ebin/",
    FrontendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, FrontendDir}, stream, use_stdio, stderr_to_stdout],
    %%FrontendSettings = [{line, ?MAX_LINE_LENGTH}, {cd, FrontendDir}, stream, use_stdio, exit_status, stderr_to_stdout],
    Frontend = open_port({spawn, ErlangExecutablePath ++ FrontendArgs}, FrontendSettings),
    timer:sleep(20000),
    FrontendPingResult = net_adm:ping('frontend_node@polygon-vm'),
    io:format("FrontendPingResult: ~p~n", [FrontendPingResult]),
    port_command(Frontend, "i?\n"),
    port_command(Frontend, "interface ?\n"),
    timer:sleep(60000),
    %%c:flush(),
    Messages = message_reader:read_all_messages(),
    io:format("Messages: ~p~n", [Messages]),
    port_close(Backend),
    port_close(Frontend).
