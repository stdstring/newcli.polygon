%% @author stdstring

-module(code_generator_tests).

-include_lib("eunit/include/eunit.hrl").
-include("code_generator_defs.hrl").
-include("frame_defs.hrl").

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