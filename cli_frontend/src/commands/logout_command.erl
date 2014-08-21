%% @author std-string

-module(logout_command).

-behaviour(command_behaviour).

-include("common_defs.hrl").

-define(LOGOUT, "logout").
-define(LOGOUT_MESSAGE, "You are logged out\n").
-define(ARG_COUNT_MISMATCH, "Argument count mismatch\n").

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_name/0, get_command_body/0, execute/3]).
%% proc_lib export
-export([execute_impl/3]).

-spec get_name() -> atom().
get_name() -> logout_command.

-spec get_command_body() -> [string()].
get_command_body() -> [?LOGOUT].

-spec execute(CommandLineRest :: string(), ClientHandler :: pid(), ExecutionState :: #execution_state{}) -> CommandPid :: pid().
execute(CommandLineRest, ClientHandler, ExecutionState) ->
    proc_lib:start_link(?MODULE, execute_impl, [CommandLineRest, ClientHandler, ExecutionState]).

-spec execute_impl(CommandLineRest :: string(), ClientHandler :: pid(), ExecutionState :: #execution_state{}) -> 'ok'.
execute_impl(CommandLineRest, ClientHandler, ExecutionState) ->
    {ReturnCode, NewExecutionState} = process_command(CommandLineRest, ClientHandler, ExecutionState),
    send_operation_result_message(ReturnCode, ClientHandler),
    client_handler:finish_command(ClientHandler, NewExecutionState, ReturnCode),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_command(CommandLineRest :: string(), ClientHandler :: pid(), ExecutionState :: #execution_state{}) ->
    {ReturnCode :: integer(), ExecutionState :: #execution_state{}}.
process_command(CommandLineRest, ClientHandler, ExecutionState) ->
    case commandline_parser:get_tokens(CommandLineRest) of
        [?LOGOUT] -> backend_command_inner:execute("logout", ClientHandler, ExecutionState);
        _Other ->
            client_handler:send_error(ClientHandler, ?ARG_COUNT_MISMATCH),
            {255, ExecutionState}
    end.

-spec send_operation_result_message(ReturnCode :: integer(), ClientHandler :: pid()) -> 'ok'.
send_operation_result_message(0, ClientHandler) ->
    client_handler:send_output(ClientHandler, ?LOGOUT_MESSAGE),
    ok;
send_operation_result_message(_ReturnCode, _ClientHandler) -> ok.
