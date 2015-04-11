%% @author std-string

-module(integration_tests_common).

-include_lib("eunit/include/eunit.hrl").

-export([prepare_cli_service_data/0,
         start_cli_service/0,
         stop_cli_service/1,
         create_tests_entry/1,
         clear_abnormal_execution/0,
         process/3,
         check_normal_execution/0]).

-include("integration_tests_defs.hrl").

-define(MAX_LINE_LENGTH, 1000).
-define(CRASH_DUMP_FILES, ["./" ++ ?CRASH_DUMP_FILE, "./" ++ ?SERVICE_BIN ++ "/" ++ ?CRASH_DUMP_FILE]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec prepare_cli_service_data() -> 'ok'.
prepare_cli_service_data() ->
    %% TODO (std_string) : think about using wildcards; see filelib:wildcard
    {ok, _} = file:copy("service_data/authentication_data", "/tmp/authentication_data"),
    {ok, _} = file:copy("service_data/authorization_data", "/tmp/authorization_data"),
    {ok, _} = file:copy("service_data/cli_fsm_data", "/tmp/cli_fsm_data"),
    {ok, _} = file:copy("service_data/command_data", "/tmp/command_data"),
    {ok, _} = file:copy("service_data/cli_service.conf", "/tmp/cli_service.conf"),
    ok.

-spec start_cli_service() -> port().
start_cli_service() ->
    {ok, CurrentDir} = file:get_cwd(),
    ErlangExecutablePath = os:find_executable("erl"),
    ServiceArgs = string_utils:format(?SERVICE_ARGS, [?SERVICE_NODE]),
    ServiceDir = filename:join([CurrentDir, ?SERVICE_BIN]),
    ServiceSettings = [{line, ?MAX_LINE_LENGTH}, {cd, ServiceDir}, stream, use_stdio, exit_status, stderr_to_stdout],
    Service = open_port({spawn, ErlangExecutablePath ++ ServiceArgs}, ServiceSettings),
    true = wait_process(?SERVICE_NODE, ?SERVICE_PROCESS, 10, 500),
    Service.

-spec stop_cli_service(Service :: port()) -> 'ok'.
stop_cli_service(Service) ->
    port_close(Service),
    rpc:call(?SERVICE_NODE, init, stop, []),
    wait_node_exit(?SERVICE_NODE, 10, 500),
    ok.

-spec create_tests_entry(Source :: {Description :: string(), Input :: string() | [string()], Output :: [string()]}) ->
    {'foreach', fun(() -> #integration_test_state{}), fun((#integration_test_state{}) -> 'ok'), [fun((#integration_test_state{}) -> 'ok')]}.
create_tests_entry(Source) ->
    MapFun = fun({Description, Input, Output}) -> create_tests_instantiator(Description, Input, Output) end,
    InstantiatorList = lists:map(MapFun, Source),
    {foreach, fun integration_tests_manager:setup/0, fun integration_tests_manager:cleanup/1, InstantiatorList}.

-spec clear_abnormal_execution() -> 'ok'.
clear_abnormal_execution() ->
    lists:foreach(fun(Filename) -> file:delete(Filename) end, ?CRASH_DUMP_FILES),
    ok.

-spec process(Input :: string() | [string()], ExpectedOutput :: [string()], State :: #integration_test_state{}) ->
    'ok' | no_return().
process([Head | _] = Input, ExpectedOutput, #integration_test_state{} = State) when is_list(Head) ->
    InputData = string:join(Input, "\n") ++ "\n",
    process(InputData, ExpectedOutput, State);
process([Char | _] = Input, ExpectedOutput, #integration_test_state{terminal_cmd = TerminalCmd}) when is_integer(Char) ->
    ?assertEqual(ok, file:write_file(?INPUT_DATA, Input)),
    OutputData = os:cmd(TerminalCmd),
    OutputDataParts = string:tokens(OutputData, "\n"),
    ?assertEqual(length(ExpectedOutput), length(OutputDataParts)),
    ActualOutput = lists:sublist(OutputDataParts, length(ExpectedOutput)),
    ?assertEqual(ExpectedOutput, ActualOutput),
    check_normal_execution(),
    ok.

-spec check_normal_execution() -> 'ok'.
check_normal_execution() ->
    lists:foreach(fun(Filename) -> ?assertNot(filelib:is_regular(Filename)) end, ?CRASH_DUMP_FILES),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec wait_process(Node :: atom(), Process :: atom(), Count :: integer(), WaitTime :: integer()) -> boolean().
wait_process(Node, Process, 0, _WaitTime) ->
    case rpc:call(Node, erlang, whereis, [Process]) of
        {badrpc, nodedown} -> false;
        undefined -> false;
        _Pid -> true
    end;
wait_process(Node, Process, Count, WaitTime) ->
    case rpc:call(Node, erlang, whereis, [Process]) of
        {badrpc, nodedown} ->
            timer:sleep(WaitTime),
            wait_process(Node, Process, Count-1, WaitTime);
        undefined ->
            timer:sleep(WaitTime),
            wait_process(Node, Process, Count-1, WaitTime);
        _Pid -> true
    end.

-spec wait_node_exit(Node :: atom(), Count :: integer(), WaitTime :: integer()) -> boolean().
wait_node_exit(Node, 0, _WaitTime) ->
    case net_adm:ping(Node) of
        pang -> true;
        pong -> false
    end;
wait_node_exit(Node, Count, WaitTime) ->
    case net_adm:ping(Node) of
        pong ->
            timer:sleep(WaitTime),
            wait_node_exit(Node, Count-1, WaitTime);
        pang -> true
    end.

-spec create_tests_instantiator(Description :: string(), Input :: string() | [string()], Output :: [string()]) ->
    fun((#integration_test_state{}) -> 'ok').
create_tests_instantiator(Description, Input, Output) ->
    fun(State) -> [{Description, fun() -> process(Input, Output, State) end}] end.