%% @author std-string

-module(backend_command).

-behaviour(command_behaviour).

-include("message_defs.hrl").
-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_name/0, get_command_body/0, execute/2]).

-spec get_name() -> atom().
get_name() -> backend_command.

-spec get_command_body() -> [string()].
get_command_body() -> [].

%%-spec create(CommandLineRest :: string()) -> {'ok', Command :: pid()} | {'error', Reason :: term()}.
%%create(CommandLineRest) -> {error, not_implemented}.

-spec execute(CommandLineRest :: string(), ExecutionState :: #execution_state{}) -> {ReturnCode :: integer(), ExecutionState :: #execution_state{}}.
execute(CommandLineRest, ExecutionState) ->
    case ExecutionState#execution_state.session of
        undefined ->
            io:format(standard_error, "Can't execute command for unauthenticated user", []),
            {255, ExecutionState};
        Session ->
            {CompletionCode, CliMode} = execute_impl(Session, CommandLineRest),
            NewExecutionState = ExecutionState#execution_state{current_cli_mode = CliMode},
            {CompletionCode, NewExecutionState}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

execute_impl(Session, CommandLineRest) ->
    gen_server:call(Session, CommandLineRest),
    receive
        #command_output{message = Message} ->
            io:format("~s", [Message]),
            execute_impl(Session, CommandLineRest);
        #command_error{message = Message} ->
            io:format(standard_error, "~s", [Message]),
            execute_impl(Session, CommandLineRest);
        #command_end{completion_code = CompletionCode, cli_mode = CliMode} ->
            {CompletionCode, CliMode};
        #command_fail{reason = Reason, cli_mode = CliMode} ->
            io:format(standard_error, "Command's execution is failed due to the following: ~p~n", [Reason]),
            {255, CliMode}
    end.

