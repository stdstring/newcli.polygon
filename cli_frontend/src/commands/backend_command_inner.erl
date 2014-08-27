%% @author std-string

-module(backend_command_inner).

-include("backend_message_defs.hrl").
-include("common_defs.hrl").

-define(EXEC_FOR_UNAUTH, "Can't execute command for unauthenticated user\n").
-define(EXEC_FAILED, "Command's execution is failed due to the following: ~w\n").

%% ====================================================================
%% API functions
%% ====================================================================

-export([execute/3]).

-spec execute(CommandLineRest :: string(), ClientHandler :: pid(), ExecutionState :: #execution_state{}) ->
    {ReturnCode :: integer(), ExecutionState :: #execution_state{}}.
execute(CommandLineRest, ClientHandler, ExecutionState) ->
    io:format("backend_command_inner:execute/3 ~n", []),
    case ExecutionState#execution_state.session of
        undefined ->
            io:format("backend_command_inner:execute/3, without session ~n", []),
            client_handler:send_error(ClientHandler, ?EXEC_FOR_UNAUTH),
            {255, ExecutionState};
        Session ->
            io:format("backend_command_inner:execute/3, with session ~n", []),
            {ReturnCode, CliMode} = process_command(Session, CommandLineRest, ClientHandler),
            NewExecutionState = ExecutionState#execution_state{current_cli_mode = CliMode},
            {ReturnCode, NewExecutionState}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_command(Session :: pid(), CommandLineRest :: string(), ClientHandler :: pid()) -> {ReturnCode :: integer, CliMode :: string()}.
process_command(Session, CommandLineRest, ClientHandler) ->
    io:format("backend_command_inner:process_command/3 ~n", []),
    io:format("backend_command_inner:process_command/3, Session: ~p~n", [Session]),
    io:format("backend_command_inner:process_command/3, CommandLineRest: ~p~n", [CommandLineRest]),
    %% TODO (std_string) : in future, create interface layer and use it
    Result = gen_server:call(Session, #command{message = CommandLineRest}),
    io:format("backend_command_inner:process_command/3, Result: ~p~n", [Result]),
    process_response(ClientHandler).

-spec process_response(ClientHandler :: pid()) -> {ReturnCode :: integer, CliMode :: string()}.
process_response(ClientHandler) ->
    receive
        #command_output{message = Message} ->
            io:format("backend_command_inner:process_response/3, output message: ~p~n", [Message]),
            client_handler:send_output(ClientHandler, Message),
            process_response(ClientHandler);
        #command_error{message = Message} ->
            io:format("backend_command_inner:process_response/3, error message: ~p~n", [Message]),
            client_handler:send_error(ClientHandler, Message),
            process_response(ClientHandler);
        #command_end{completion_code = ReturnCode, cli_mode = CliMode} ->
            io:format("backend_command_inner:process_response/3, end message: ~p~n", [ReturnCode]),
            {ReturnCode, CliMode};
        #command_fail{reason = Reason, cli_mode = CliMode} ->
            io:format("backend_command_inner:process_response/3, fail message: ~p~n", [Reason]),
            Message = message_helper:format(?EXEC_FAILED, [Reason]),
            client_handler:send_error(ClientHandler, Message),
            {255, CliMode};
        Other ->
            io:format("backend_command_inner:process_response/3, Other message: ~p~n", [Other]),
            process_response(ClientHandler)
    end.
