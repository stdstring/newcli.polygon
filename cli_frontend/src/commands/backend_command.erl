%% @author std-string

-module(backend_command).

-behaviour(command_behaviour).

-include("backend_message_defs.hrl").
-include("common_defs.hrl").

-define(EXEC_FOR_UNAUTH, "Can't execute command for unauthenticated user\n").
-define(EXEC_FAILED, "Command's execution is failed due to the following: ~w\n").

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_name/0, get_command_body/0, execute/3]).
%% proc_lib export
-export([execute_impl/3]).

-spec get_name() -> atom().
get_name() -> backend_command.

-spec get_command_body() -> [string()].
get_command_body() -> [].

-spec execute(CommandLineRest :: string(), ClientHandler :: pid(), ExecutionState :: #execution_state{}) -> CommandPid :: pid() | {'error', Reason :: term()}.
execute(CommandLineRest, ClientHandler, ExecutionState) ->
    proc_lib:start_link(?MODULE, execute_impl, [CommandLineRest, ClientHandler, ExecutionState]).

execute_impl(CommandLineRest, ClientHandler, ExecutionState) ->
    proc_lib:init_ack(self()),
    case ExecutionState#execution_state.session of
        undefined ->
            client_handler:send_error(ClientHandler, ?EXEC_FOR_UNAUTH),
            client_handler:finish_command(ClientHandler, ExecutionState, 255);
        Session ->
            {ReturnCode, CliMode} = process_command(Session, CommandLineRest, ClientHandler),
            NewExecutionState = ExecutionState#execution_state{current_cli_mode = CliMode},
            client_handler:finish_command(ClientHandler, NewExecutionState, ReturnCode)
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
