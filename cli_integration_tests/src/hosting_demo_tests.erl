-module(hosting_demo_tests).

-include_lib("eunit/include/eunit.hrl").

-include("integration_tests_defs.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

separate_services_test() ->
    {ok, CurrentDir} = file:get_cwd(),
    TerminalCmd = filename:join([CurrentDir, ?CLI_TERMINAL_BIN, ?CLI_TERMINAL_EXEC]) ++ ?CLI_TERMINAL_ARGS,
    start_cli_service(),
    execute(TerminalCmd, ["root", "iddqd", "ping XXX", "logout"]),
    stop_cli_service(),
    start_cli_service(),
    execute(TerminalCmd, ["guest", "idclip", "ping XXX", "logout"]),
    stop_cli_service(),
    ok.

one_service_test() ->
    {ok, CurrentDir} = file:get_cwd(),
    TerminalCmd = filename:join([CurrentDir, ?CLI_TERMINAL_BIN, ?CLI_TERMINAL_EXEC]) ++ ?CLI_TERMINAL_ARGS,
    start_cli_service(),
    execute(TerminalCmd, ["root", "iddqd", "ping XXX", "logout"]),
    execute(TerminalCmd, ["guest", "idclip", "ping XXX", "logout"]),
    stop_cli_service(),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

execute(TerminalCmd, Input) ->
    InputData = string:join(Input, "\n") ++ "\n",
    ?assertEqual(ok, file:write_file(?INPUT_DATA, InputData)),
    OutputData = os:cmd(TerminalCmd),
    ?assertNotEqual("", OutputData).

start_cli_service() ->
    ?assertEqual(ok, application:start(cli_service_application)).

stop_cli_service() ->
    ?assertEqual(ok, application:stop(cli_service_application)).
