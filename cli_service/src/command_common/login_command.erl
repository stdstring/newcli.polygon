%% @author std-string

-module(login_command).

-behaviour(command_behaviour).

-export([get_name/0, get_command_body/0, get_help/0, execute/4]).

-include("authentication_defs.hrl").
-include("command_defs.hrl").
-include("crypto_defs.hrl").

%% TODO (std_string) : move into common place
-define(BAD_ARGS_MESSAGE, "Bad arguments").
-define(BAD_ARGS_CODE, 255).
-define(ALREADY_LOGGED_MESSAGE, "User is already logged").
-define(ALREADY_LOGGED_CODE, 255).
-define(UNKNOWN_USER_MESSAGE, "Login's attempt is failed due to the following: unknown user").
-define(UNKNOWN_USER_CODE, 255).
-define(BAD_PASSWORD_MESSAGE, "Login's attempt is failed due to the following: bad password").
-define(BAD_PASSWORD_CODE, 255).
-define(LOGIN_FAILED_MESSAGE, "Login's attempt is failed").
-define(LOGIN_FAILED_CODE, 255).
-define(DEFAULT_GREETING, "Default greeting message").

%% ====================================================================
%% API functions
%% ====================================================================

get_name() -> login.

get_command_body() -> ["login"].

get_help() -> "login help".

execute(Args, Stdout, Stderr, ExecContext) ->
    case check_args(Args) of
        false -> process_error(Stderr, ?BAD_ARGS_MESSAGE, ?BAD_ARGS_CODE, ExecContext);
        true ->
            [Username, Password] = Args,
            process_command(Username, Password, Stdout, Stderr, ExecContext)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec check_args(Args :: [term()]) -> boolean().
check_args([_Username, _Password]) -> true;
check_args(_Other) -> false.

process_command(Username, PwdString, Stdout, Stderr, ExecContext) ->
    case lists:keyfind(?USER_KEY, 1, ExecContext) of
        false ->
            Pwd = create_pwd_hash(base64:decode_to_string(PwdString)),
            Result = authentication_service:authenticate(Username, Pwd),
            process_authentiaction_result(Result, Stdout, Stderr, ExecContext);
        {?USER_KEY, _User} ->
            process_error(Stderr, ?ALREADY_LOGGED_MESSAGE, ?ALREADY_LOGGED_CODE, ExecContext)
    end.

-spec process_authentiaction_result(Result :: {'authentication_complete', User :: #user{}} | {'authentication_fail', Reason :: term()},
                                    Stdout :: pid(),
                                    Stderr :: pid(),
                                    ExecContext :: [{Key :: atom(), Value :: term()}]) ->
    {ReturnValue :: integer(), ExecContext :: [{Key :: atom(), Value :: term()}]}.
process_authentiaction_result({authentication_complete, User}, Stdout, _Stderr, ExecContext) ->
    GreetingMessage = list_utils:get_value_by_key_with_default(ExecContext, ?GREETING_MESSAGE_KEY, 1, ?DEFAULT_GREETING),
    send_output(Stdout, GreetingMessage),
    NewExecContext = lists:keystore(?USER_KEY, 1, ExecContext, {?USER_KEY, User}),
    {0, NewExecContext};
process_authentiaction_result({authentication_fail, unknown_username}, _Stdout, Stderr, ExecContext) ->
    process_error(Stderr, ?UNKNOWN_USER_MESSAGE, ?UNKNOWN_USER_CODE, ExecContext);
process_authentiaction_result({authentication_fail, bad_password}, _Stdout, Stderr, ExecContext) ->
    process_error(Stderr, ?BAD_PASSWORD_MESSAGE, ?BAD_PASSWORD_CODE, ExecContext);
process_authentiaction_result({authentication_fail, _Reason}, _Stdout, Stderr, ExecContext) ->
    process_error(Stderr, ?LOGIN_FAILED_MESSAGE, ?LOGIN_FAILED_CODE, ExecContext).

%% TODO (std_string) : move into common place
-spec process_error(Stderr :: pid(), Message :: string(), ReturnCode :: integer(), ExecContext :: [{Key :: atom(), Value :: term()}]) ->
    {ReturnValue :: integer(), ExecContext :: [{Key :: atom(), Value :: term()}]}.
process_error(Stderr, Message, ReturnCode, ExecContext) ->
    send_error(Stderr, Message),
    {ReturnCode, ExecContext}.

%% TODO (std_string) : move into common place
-spec send_output(Stdout :: pid(), Message :: string()) -> 'ok'.
send_output(Stdout, Message) ->
    gen_server:call(Stdout, {output, Message}),
    ok.

%% TODO (std_string) : move into common place
-spec send_error(Stderr :: pid(), Message :: string()) -> 'ok'.
send_error(Stderr, Message) ->
    gen_server:call(Stderr, {error, Message}),
    ok.

-spec create_pwd_hash(PwdString :: string()) -> binary().
create_pwd_hash(PwdString) ->
    crypto_utils:hash(?HASH_TYPE, PwdString, ?HASH_SALT).