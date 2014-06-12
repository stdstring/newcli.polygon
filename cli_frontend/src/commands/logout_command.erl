%% @author std-string

-module(logout_command).

-behaviour(command_behaviour).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_name/0, get_command_body/0, execute/2]).

-spec get_name() -> atom().
get_name() -> logout_command.

-spec get_command_body() -> [string()].
get_command_body() -> ["logout"].

-spec execute(CommandLineRest :: string(), ExecutionState :: #execution_state{}) -> {ReturnCode :: integer(), ExecutionState :: #execution_state{}}.
execute("", ExecutionState) ->
    {ReturnCode, NewExecutionState} = backend_command:execute("logout", ExecutionState),
    show_operation_result_message(ReturnCode),
    {ReturnCode, NewExecutionState#execution_state{session = undefined, login_info = undefined, current_cli_mode = undefined}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

- spec show_operation_result_message(ReturnCode :: integer()) -> 'ok'.
show_operation_result_message(0) ->
    io:format("You are logged out.~n", []),
    ok;
show_operation_result_message(_ReturnCode) -> ok.
