%% @author std-string

-module(backend_command).

-behaviour(command_behaviour).

-include("backend_message_defs.hrl").
-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_name/0, get_command_body/0, execute/3]).

-spec get_name() -> atom().
get_name() -> backend_command.

-spec get_command_body() -> [string()].
get_command_body() -> [].

-spec execute(CommandLineRest :: string(), ClientHandler :: pid(), ExecutionState :: #execution_state{}) -> CommandPid :: pid().
execute(_CommandLineRest, _ClientHandler, _ExecutionState) -> undefined.

%%-spec execute(CommandLineRest :: string(), ExecutionState :: #execution_state{}) -> {ReturnCode :: integer(), ExecutionState :: #execution_state{}}.
%%execute(CommandLineRest, ExecutionState) ->
%%    case ExecutionState#execution_state.session of
%%        undefined ->
%%            io:format(standard_error, "Can't execute command for unauthenticated user.~n", []),
%%            {255, ExecutionState};
%%        Session ->
%%            {ReturnCode, CliMode} = execute_impl(Session, CommandLineRest),
%%            NewExecutionState = ExecutionState#execution_state{current_cli_mode = CliMode},
%%            {ReturnCode, NewExecutionState}
%%    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%%-spec execute_impl(Session :: pid(), CommandLineRest :: string()) -> {ReturnCode :: integer, CliMode :: string()}.
%%execute_impl(Session, CommandLineRest) ->
%%    gen_server:call(Session, #command{message = CommandLineRest}),
%%    process_response().

%%-spec process_response() -> {ReturnCode :: integer, CliMode :: string()}.
%%process_response() ->
%%    receive
%%        #command_output{message = Message} ->
%%            io:format("~s", [Message]),
%%            process_response();
%%        #command_error{message = Message} ->
%%            io:format(standard_error, "~s", [Message]),
%%            process_response();
%%        #command_end{completion_code = ReturnCode, cli_mode = CliMode} ->
%%            {ReturnCode, CliMode};
%%        #command_fail{reason = Reason, cli_mode = CliMode} ->
%%            io:format(standard_error, "Command's execution is failed due to the following: ~w~n", [Reason]),
%%            {255, CliMode}
%%    end.
