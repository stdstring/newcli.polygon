%% @author std-string

-module(backend_command_inner).

-export([execute/3]).

-include("backend_message_defs.hrl").
-include("common_defs.hrl").

-define(EXEC_FOR_UNAUTH, "Can't execute command for unauthenticated user\n").
-define(EXEC_FAILED, "Command's execution is failed due to the following: ~w\n").

%% ====================================================================
%% API functions
%% ====================================================================

-spec execute(CommandLineRest :: string(), ClientHandler :: pid(), ExecutionState :: #execution_state{}) ->
    {ReturnCode :: integer(), ExecutionState :: #execution_state{}}.
execute(CommandLineRest, ClientHandler, ExecutionState) ->
    case ExecutionState#execution_state.session of
        undefined ->
            client_handler:send_error(ClientHandler, ?EXEC_FOR_UNAUTH),
            {255, ExecutionState};
        Session ->
            {ReturnCode, CliMode} = process_command(Session, CommandLineRest, ClientHandler),
            NewExecutionState = ExecutionState#execution_state{current_cli_mode = CliMode},
            {ReturnCode, NewExecutionState}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_command(Session :: pid(), CommandLineRest :: string(), ClientHandler :: pid()) -> {ReturnCode :: integer, CliMode :: string()}.
process_command(Session, CommandLineRest, ClientHandler) ->
    %% TODO (std_string) : in future, create interface layer and use it
    gen_server:call(Session, #command{message = CommandLineRest}),
    process_response(ClientHandler).

-spec process_response(ClientHandler :: pid()) -> {ReturnCode :: integer, CliMode :: string()}.
process_response(ClientHandler) ->
    receive
        #command_output{message = Message} ->
            client_handler:send_output(ClientHandler, Message),
            process_response(ClientHandler);
        #command_error{message = Message} ->
            client_handler:send_error(ClientHandler, Message),
            process_response(ClientHandler);
        #command_end{completion_code = ReturnCode, cli_mode = CliMode} ->
            {ReturnCode, CliMode};
        #command_fail{reason = Reason, cli_mode = CliMode} ->
            Message = lists:flatten(io_lib:format(?EXEC_FAILED, [Reason])),
            client_handler:send_error(ClientHandler, Message),
            {255, CliMode}
    end.
