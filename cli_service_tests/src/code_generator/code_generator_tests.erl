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

-record(test_state, {binary = <<>> :: binary()}).

%% ====================================================================
%% Test functions
%% ====================================================================

%%generate_test_() ->
%%    %%Tests = [{"interaction with backend", fun() -> check_backend() end}, {"interaction with frontend", fun() -> check_frontend() end}],
%%    Tests = [{"test", fun(State) -> io:format(user, "State:~p~n", [State]) end}],
%%    [{foreach, fun setup/0, fun cleanup/1, Tests}].

generate_test() ->
    State = setup(),
    CliFsmRef = cli_fsm_instance,
    ClientHandlerRef = client_handler_instance,
    User = #user{uid = 666, username = "superuser", access_level = 11},
    Expected = [#expectation{source = io_buffer, func = start, args = [], result = {error, enotsup}},
                #expectation{source = client_handler, func = send_error, args = [client_handler_instance, "Buffer creation fails"], result = ok},
                #expectation{source = client_handler, func = finish_command, args = [client_handler_instance, 255], result = ok}],
    mock_server:set_expected(Expected),
    {module, ?ENTRY_MODULE} = code:load_binary(?ENTRY_MODULE, [], State#test_state.binary),
    Result = ?ENTRY_MODULE:?ENTRY_FUNC(CliFsmRef, ClientHandlerRef, User),
    io:format(user, "Result: ~p~n", [Result]),
    cleanup(State),
    ok.

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