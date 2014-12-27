%% @author stdstring

-module(code_generator_tests).

-include_lib("eunit/include/eunit.hrl").

-include("code_generator_defs.hrl").
-include("frame_defs.hrl").
-include("mock_defs.hrl").
-include("authentication_defs.hrl").
-include("command_defs.hrl").

-define(TEST_ENTRY_MODULE, test_entry_module).
-define(TEST_ENTRY_FUNC, test_entry_func).
-define(COMMAND_ARGS, [#argument{type = word, value = "iddqd"}, #argument{type = string, value = "666"}]).
-define(COMMAND, #command{module = command_mock, arguments = ?COMMAND_ARGS}).
-define(BUFFER_REF, io_buffer_instance).
-define(CLI_FSM_REF, cli_fsm_instance).
-define(CLIENT_HANDLER_REF, client_handler_instance).
-define(USER, #user{uid = 666, username = "superuser", access_level = 11}).
-define(CONTEXT_WITHOUT_USER, [{some_key, some_value}]).
-define(CONTEXT, [{?USER_KEY, ?USER}]).
-define(CHANGED_CONTEXT, [{?USER_KEY, ?USER}, {some_key, some_value}]).

-record(test_state, {binary = <<>> :: binary()}).

%% ====================================================================
%% Test functions
%% ====================================================================

compilation_test() ->
    ModuleDefs = #module_defs{io_buffer_module = io_buffer_mock,
                             client_handler_module = client_handler_mock,
                             cli_fsm_module = cli_fsm_mock,
                             exec_checker_module = command_execution_checker_mock},
    {true, Binary} = code_generator:generate(?TEST_ENTRY_MODULE, ?TEST_ENTRY_FUNC, ?COMMAND, ModuleDefs),
    ?assert(is_binary(Binary)).

generate_test_() ->
    Tests = [fun(State) ->
        [{"test buffer creation error",
          ?_test(check(create_buffer_error_exps(?CONTEXT), {255, ?CONTEXT}, State, ?CONTEXT))},
         {"test unsuitable command error without user",
          ?_test(check(create_unsuitable_command_exps(?CONTEXT_WITHOUT_USER), {255, ?CONTEXT_WITHOUT_USER}, State, ?CONTEXT_WITHOUT_USER))},
         {"test unsuitable command error",
          ?_test(check(create_unsuitable_command_exps(?CONTEXT), {255, ?CONTEXT}, State, ?CONTEXT))},
         {"test access denied error without user",
          ?_test(check(create_access_denied_exps(?CONTEXT_WITHOUT_USER), {255, ?CONTEXT_WITHOUT_USER}, State, ?CONTEXT_WITHOUT_USER))},
         {"test access denied error",
          ?_test(check(create_access_denied_exps(?CONTEXT), {255, ?CONTEXT}, State, ?CONTEXT))},
         {"test bad config error without user",
          ?_test(check(create_bad_config_exps(?CONTEXT_WITHOUT_USER), {255, ?CONTEXT_WITHOUT_USER}, State, ?CONTEXT_WITHOUT_USER))},
         {"test bad config error",
          ?_test(check(create_bad_config_exps(?CONTEXT), {255, ?CONTEXT}, State, ?CONTEXT))},
         {"test fail command execution without user",
          ?_test(check(create_fail_command_exps(?CONTEXT_WITHOUT_USER), {128, ?CONTEXT_WITHOUT_USER}, State, ?CONTEXT_WITHOUT_USER))},
         {"test fail command execution",
          ?_test(check(create_fail_command_exps(?CONTEXT), {128, ?CONTEXT}, State, ?CONTEXT))},
         {"test fail command execution with change context",
          ?_test(check(create_fail_command_exps(?CONTEXT, ?CHANGED_CONTEXT), {128, ?CHANGED_CONTEXT}, State, ?CONTEXT))},
         {"test success command execution without user",
          ?_test(check(create_success_command_exps(?CONTEXT_WITHOUT_USER), {0, ?CONTEXT_WITHOUT_USER}, State, ?CONTEXT_WITHOUT_USER))},
         {"test success command execution",
          ?_test(check(create_success_command_exps(?CONTEXT), {0, ?CONTEXT}, State, ?CONTEXT))},
         {"test success command execution with change context",
          ?_test(check(create_success_command_exps(?CONTEXT, ?CHANGED_CONTEXT), {0, ?CHANGED_CONTEXT}, State, ?CONTEXT))}]
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
    {true, Binary} = code_generator:generate(?TEST_ENTRY_MODULE, ?TEST_ENTRY_FUNC, ?COMMAND, ModuleDefs),
    #test_state{binary = Binary}.

cleanup(_State) ->
    mock_server:stop().

check(ExpectedBehavior, ExpectedResult, State, Context) ->
    mock_server:set_expected(ExpectedBehavior),
    {module, ?TEST_ENTRY_MODULE} = code:load_binary(?TEST_ENTRY_MODULE, [], State#test_state.binary),
    ActualResult = ?TEST_ENTRY_MODULE:?TEST_ENTRY_FUNC(?CLI_FSM_REF, ?CLIENT_HANDLER_REF, Context),
    ?assertEqual(ExpectedResult, ActualResult),
    mock_server:check_finish().

create_buffer_error_exps(Context) ->
    [#expectation{source = io_buffer, func = start, args = [], result = {error, enotsup}},
     #expectation{source = client_handler, func = send_error, args = [?CLIENT_HANDLER_REF, ?BUFFER_START_FAIL_MESSAGE], result = ok},
     #expectation{source = client_handler, func = finish_exec, args = [?CLIENT_HANDLER_REF, 255, Context], result = ok}].

create_unsuitable_command_exps(Context) ->
    User = list_utils:get_value_by_key_with_default(Context, ?USER_KEY, 1, undefined),
    [#expectation{source = io_buffer, func = start, args = [], result = {ok, ?BUFFER_REF}},
     #expectation{source = command_execution_checker, func = execution_precheck, args = [command_example, ?CLI_FSM_REF, User], result = {false, unsuitable_command}},
     #expectation{source = io_buffer, func = get_data, args = [?BUFFER_REF, both], result = []},
     #expectation{source = client_handler, func = send_error, args = [?CLIENT_HANDLER_REF, ?UNSUITABLE_COMMAND_MESSAGE], result = ok},
     #expectation{source = client_handler, func = finish_exec, args = [?CLIENT_HANDLER_REF, 255, Context], result = ok}].

create_access_denied_exps(Context) ->
    User = list_utils:get_value_by_key_with_default(Context, ?USER_KEY, 1, undefined),
    [#expectation{source = io_buffer, func = start, args = [], result = {ok, ?BUFFER_REF}},
     #expectation{source = command_execution_checker, func = execution_precheck, args = [command_example, ?CLI_FSM_REF, User], result = {false, access_denied}},
     #expectation{source = io_buffer, func = get_data, args = [?BUFFER_REF, both], result = []},
     #expectation{source = client_handler, func = send_error, args = [?CLIENT_HANDLER_REF, ?ACCESS_DENIED_MESSAGE], result = ok},
     #expectation{source = client_handler, func = finish_exec, args = [?CLIENT_HANDLER_REF, 255, Context], result = ok}].

create_bad_config_exps(Context) ->
    User = list_utils:get_value_by_key_with_default(Context, ?USER_KEY, 1, undefined),
    [#expectation{source = io_buffer, func = start, args = [], result = {ok, ?BUFFER_REF}},
     #expectation{source = command_execution_checker, func = execution_precheck, args = [command_example, ?CLI_FSM_REF, User], result = {false, authorization_bad_config}},
     #expectation{source = io_buffer, func = get_data, args = [?BUFFER_REF, both], result = []},
     #expectation{source = client_handler, func = send_error, args = [?CLIENT_HANDLER_REF, ?BAD_CONFIG_MESSAGE], result = ok},
     #expectation{source = client_handler, func = finish_exec, args = [?CLIENT_HANDLER_REF, 255, Context], result = ok}].

create_fail_command_exps(Context) ->
    create_fail_command_exps(Context, Context).

create_fail_command_exps(Context, NewContext) ->
    User = list_utils:get_value_by_key_with_default(Context, ?USER_KEY, 1, undefined),
    [#expectation{source = io_buffer, func = start, args = [], result = {ok, ?BUFFER_REF}},
     #expectation{source = command_execution_checker, func = execution_precheck, args = [command_example, ?CLI_FSM_REF, User], result = true},
     #expectation{source = command_module, func = execute, args = [["iddqd","666"], ?BUFFER_REF, ?BUFFER_REF, Context], result = {128, NewContext}},
     #expectation{source = io_buffer, func = get_data, args = [?BUFFER_REF, both], result = [{output, "IDDQD"}, {error, "IDKFA"}]},
     #expectation{source = client_handler, func = send_output, args = [?CLIENT_HANDLER_REF, "IDDQD"], result = ok},
     #expectation{source = client_handler, func = send_error, args = [?CLIENT_HANDLER_REF, "IDKFA"], result = ok},
     #expectation{source = client_handler, func = send_error, args = [?CLIENT_HANDLER_REF, ?COMMAND_FAIL_MESSAGE ++ "128"], result = ok},
     #expectation{source = client_handler, func = finish_exec, args = [?CLIENT_HANDLER_REF, 128, NewContext], result = ok}].

create_success_command_exps(Context) ->
    create_success_command_exps(Context, Context).

create_success_command_exps(Context, NewContext) ->
    User = list_utils:get_value_by_key_with_default(Context, ?USER_KEY, 1, undefined),
    [#expectation{source = io_buffer, func = start, args = [], result = {ok, ?BUFFER_REF}},
     #expectation{source = command_execution_checker, func = execution_precheck, args = [command_example, ?CLI_FSM_REF, User], result = true},
     #expectation{source = command_module, func = execute, args = [["iddqd","666"], ?BUFFER_REF, ?BUFFER_REF, Context], result = {0, NewContext}},
     #expectation{source = cli_fsm, func = process_command, args = [command_example, ?CLI_FSM_REF], result = ok},
     #expectation{source = io_buffer, func = get_data, args = [?BUFFER_REF, both], result = [{output, "IDDQD"}, {error, "IDKFA"}]},
     #expectation{source = client_handler, func = send_output, args = [?CLIENT_HANDLER_REF, "IDDQD"], result = ok},
     #expectation{source = client_handler, func = send_error, args = [?CLIENT_HANDLER_REF, "IDKFA"], result = ok},
     #expectation{source = client_handler, func = finish_exec, args = [?CLIENT_HANDLER_REF, 0, NewContext], result = ok}].