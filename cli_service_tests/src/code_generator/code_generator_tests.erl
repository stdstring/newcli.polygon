%% @author stdstring

-module(code_generator_tests).

-include_lib("eunit/include/eunit.hrl").
-include("code_generator_defs.hrl").
-include("frame_defs.hrl").
-include("mock_defs.hrl").
-include("authentication_defs.hrl").

-define(ENTRY_MODULE, test_entry_module).
-define(ENTRY_FUNC, test_entry_func).
-define(COMMAND_ARGS, [#argument{type = word, value = "iddqd"}, #argument{type = string, value = "666"}]).
-define(COMMAND, #command{module = command_mock, function = execute, arguments = ?COMMAND_ARGS}).
-define(BUFFER_REF, io_buffer_instance).
-define(CLI_FSM_REF, cli_fsm_instance).
-define(CLIENT_HANDLER_REF, client_handler_instance).
-define(USER, #user{uid = 666, username = "superuser", access_level = 11}).

-record(test_state, {binary = <<>> :: binary()}).

%% ====================================================================
%% Test functions
%% ====================================================================

generate_test_() ->
    Tests = [fun(State) ->
        [{"test buffer creation error", ?_test(check(create_expected_for_buffer_error(), true, State))},
         {"test unsuitable command error", ?_test(check(create_expected_for_unsuitable_command_error(), true, State))},
         {"test access denied error", ?_test(check(create_expected_for_access_denied_error(), true, State))},
         {"test bad config error", ?_test(check(create_expected_for_bad_config_error(), true, State))},
         {"test fail command execution", ?_test(check(create_expected_for_fail_command_exec(), true, State))},
         {"test success command execution", ?_test(check(create_expected_for_success_command_exec(), true, State))}]
    end],
    [{foreach, fun setup/0, fun cleanup/1, Tests}].

%% ====================================================================
%% Internal functions
%% ====================================================================

setup() ->
    mock_server:start(),
    ModuleDefs = #module_defs{io_buffer_module = io_buffer_mock,
                             client_handler_module = client_handler_mock,
                             cli_fsm_module = cli_fsm_mock,
                             exec_checker_module = command_execution_checker_mock},
    {true, Binary} = code_generator:generate(?ENTRY_MODULE, ?ENTRY_FUNC, ?COMMAND, ModuleDefs),
    #test_state{binary = Binary}.

cleanup(_State) ->
    mock_server:stop().

check(ExpectedBehavior, ExpectedResult, State) ->
    mock_server:set_expected(ExpectedBehavior),
    {module, ?ENTRY_MODULE} = code:load_binary(?ENTRY_MODULE, [], State#test_state.binary),
    ActualResult = ?ENTRY_MODULE:?ENTRY_FUNC(?CLI_FSM_REF, ?CLIENT_HANDLER_REF, ?USER),
    ?assertEqual(ExpectedResult, ActualResult).

create_expected_for_buffer_error() ->
    [#expectation{source = io_buffer, func = start, args = [], result = {error, enotsup}},
     #expectation{source = client_handler, func = send_error, args = [?CLIENT_HANDLER_REF, ?BUFFER_START_FAIL_MESSAGE], result = ok},
     #expectation{source = client_handler, func = finish_command, args = [?CLIENT_HANDLER_REF, 255], result = ok}].

create_expected_for_unsuitable_command_error() ->
    [#expectation{source = io_buffer, func = start, args = [], result = {ok, ?BUFFER_REF}},
     #expectation{source = command_execution_checker, func = execution_precheck, args = [command_example, ?CLI_FSM_REF, ?USER], result = {false, unsuitable_command}},
     #expectation{source = io_buffer, func = get_data, args = [?BUFFER_REF, both], result = []},
     #expectation{source = client_handler, func = send_error, args = [?CLIENT_HANDLER_REF, ?UNSUITABLE_COMMAND_MESSAGE], result = ok},
     #expectation{source = client_handler, func = finish_command, args = [?CLIENT_HANDLER_REF, 255], result = ok}].

create_expected_for_access_denied_error() ->
    [#expectation{source = io_buffer, func = start, args = [], result = {ok, ?BUFFER_REF}},
     #expectation{source = command_execution_checker, func = execution_precheck, args = [command_example, ?CLI_FSM_REF, ?USER], result = {false, access_denied}},
     #expectation{source = io_buffer, func = get_data, args = [?BUFFER_REF, both], result = []},
     #expectation{source = client_handler, func = send_error, args = [?CLIENT_HANDLER_REF, ?ACCESS_DENIED_MESSAGE], result = ok},
     #expectation{source = client_handler, func = finish_command, args = [?CLIENT_HANDLER_REF, 255], result = ok}].

create_expected_for_bad_config_error() ->
    [#expectation{source = io_buffer, func = start, args = [], result = {ok, ?BUFFER_REF}},
     #expectation{source = command_execution_checker, func = execution_precheck, args = [command_example, ?CLI_FSM_REF, ?USER], result = {false, authorization_bad_config}},
     #expectation{source = io_buffer, func = get_data, args = [?BUFFER_REF, both], result = []},
     #expectation{source = client_handler, func = send_error, args = [?CLIENT_HANDLER_REF, ?BAD_CONFIG_MESSAGE], result = ok},
     #expectation{source = client_handler, func = finish_command, args = [?CLIENT_HANDLER_REF, 255], result = ok}].

create_expected_for_fail_command_exec() ->
    [#expectation{source = io_buffer, func = start, args = [], result = {ok, ?BUFFER_REF}},
     #expectation{source = command_execution_checker, func = execution_precheck, args = [command_example, ?CLI_FSM_REF, ?USER], result = true},
     #expectation{source = command_module, func = execute, args = [["iddqd","666"]], result = 128},
     #expectation{source = io_buffer, func = get_data, args = [?BUFFER_REF, both], result = [{output, "IDDQD"}, {error, "IDKFA"}]},
     #expectation{source = client_handler, func = send_output, args = [?CLIENT_HANDLER_REF, "IDDQD"], result = ok},
     #expectation{source = client_handler, func = send_error, args = [?CLIENT_HANDLER_REF, "IDKFA"], result = ok},
     %% TODO (std_string) : wrong args !!!!
     #expectation{source = client_handler, func = send_error, args = [?CLIENT_HANDLER_REF, true], result = ok},
     #expectation{source = client_handler, func = finish_command, args = [?CLIENT_HANDLER_REF, 128], result = ok}].

create_expected_for_success_command_exec() ->
    [#expectation{source = io_buffer, func = start, args = [], result = {ok, ?BUFFER_REF}},
     #expectation{source = command_execution_checker, func = execution_precheck, args = [command_example, ?CLI_FSM_REF, ?USER], result = true},
     #expectation{source = command_module, func = execute, args = [["iddqd","666"]], result = 0},
     #expectation{source = io_buffer, func = get_data, args = [?BUFFER_REF, both], result = [{output, "IDDQD"}, {error, "IDKFA"}]},
     #expectation{source = client_handler, func = send_output, args = [?CLIENT_HANDLER_REF, "IDDQD"], result = ok},
     #expectation{source = client_handler, func = send_error, args = [?CLIENT_HANDLER_REF, "IDKFA"], result = ok},
     #expectation{source = client_handler, func = finish_command, args = [?CLIENT_HANDLER_REF, 0], result = ok},
     %% TODO (std_string) : wrong order
     #expectation{source = cli_fsm, func = process_command, args = [command_example, ?CLI_FSM_REF], result = ok}].