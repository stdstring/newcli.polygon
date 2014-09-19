%% @author std-string

-module(login_command).

-behaviour(command_behaviour).

-include("crypto_defs.hrl").
-include("backend_message_defs.hrl").
-include("common_defs.hrl").

-define(ALREADY_LOGGED, "You are already logged in.\n").
-define(ARG_COUNT_MISMATCH, "Argument count mismatch\n").
-define(LOGIN_FAILED, "Login's attempt is failed due to the following: ~w\n").

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_name/0, get_command_body/0, execute/3]).
%% proc_lib export
-export([execute_impl/3]).

-spec get_name() -> atom().
get_name() -> login_command.

-spec get_command_body() -> [string()].
get_command_body() -> ["login"].

-spec execute(CommandLineRest :: string(), ClientHandler :: pid(), ExecutionState :: #execution_state{}) -> CommandPid :: pid().
execute(CommandLineRest, ClientHandler, ExecutionState) ->
    proc_lib:start_link(?MODULE, execute_impl, [CommandLineRest, ClientHandler, ExecutionState]).

-spec execute_impl(CommandLineRest :: string(), ClientHandler :: pid(), ExecutionState :: #execution_state{}) -> 'ok'.
execute_impl(CommandLineRest, ClientHandler, ExecutionState) ->
    proc_lib:init_ack(self()),
    case ExecutionState#execution_state.session of
        undefined ->
            {ReturnCode, NewExecutionState} = process_command(CommandLineRest, ClientHandler, ExecutionState),
            client_handler:finish_command(ClientHandler, NewExecutionState, ReturnCode);
        _Session ->
            client_handler:send_error(ClientHandler, ?ALREADY_LOGGED),
            client_handler:finish_command(ClientHandler, ExecutionState, 255)
    end,
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_command(CommandLineRest :: string(), ClientHandler :: pid(), ExecutionState :: #execution_state{}) ->
    {ReturnCode :: integer(), ExecutionState :: #execution_state{}}.
process_command(CommandLineRest, ClientHandler, ExecutionState) ->
    case commandline_parser:get_tokens(CommandLineRest) of
        [Login, PwdString] ->
            Pwd = create_pwd_hash(base64:decode_to_string(PwdString)),
            LoginCommand = #login{login_name = Login, password = Pwd},
            BackendGlobalHandler = ExecutionState#execution_state.global_handler,
            %% TODO (std_string) : in future, create interface layer and use it
            LoginResponse = gen_server:call(BackendGlobalHandler, LoginCommand),
            process_login_response(LoginResponse, ClientHandler, ExecutionState);
        _Other ->
            client_handler:send_error(ClientHandler, ?ARG_COUNT_MISMATCH),
            {255, ExecutionState}
    end.

-spec process_login_response(Response :: #login_success{} | #login_fail{}, ClientHandler :: pid(), ExecutionState :: #execution_state{}) ->
    {ReturnCode :: integer(), ExecutionState :: #execution_state{}}.
process_login_response(Response, ClientHandler, ExecutionState) ->
    case Response of
        #login_success{login_name = LoginName, is_admin = IsAdmin, session_pid = Session, greeting = Greeting} ->
            GreetingMessage = string_data_utils:add_trailing_line_feed(Greeting),
            client_handler:send_output(ClientHandler, GreetingMessage),
            LoginInfo = #login_info{login_name = LoginName, is_admin = IsAdmin},
            CliMode = "",
            NewExecutionState = ExecutionState#execution_state{session = Session, login_info = LoginInfo, current_cli_mode = CliMode},
            {0, NewExecutionState};
        #login_fail{reason = Reason} ->
            Message = message_helper:format(?LOGIN_FAILED, [Reason]),
            client_handler:send_error(ClientHandler, Message),
            {255, ExecutionState}
    end.

-spec create_pwd_hash(PwdString :: string()) -> binary().
create_pwd_hash(PwdString) ->
    crypto_utils:hash(?HASH_TYPE, PwdString, ?HASH_SALT).