%% @author std-string

-module(login_command).

-include("crypto_defs.hrl").
-include("message_defs.hrl").
-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_name/0, get_command_body/0, execute/2]).

-spec get_name() -> atom().
get_name() -> login_command.

-spec get_command_body() -> [string()].
get_command_body() -> ["login"].

%%-spec create(CommandLineRest :: string()) -> {'ok', Command :: pid()} | {'error', Reason :: term()}.
%%create(CommandLineRest) -> {error, not_implemented}.

-spec execute(CommandLineRest :: string(), ExecutionState :: #execution_state{}) -> {ReturnCode :: integer(), ExecutionState :: #execution_state{}}.
execute("", ExecutionState) ->
    case ExecutionState#execution_state.session of
        undefined -> execute_impl(ExecutionState);
        _Session ->
            io:format(standard_error, "You are already logged in.~n", []),
            {255, ExecutionState}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec execute_impl(ExecutionState :: #execution_state{}) -> {ReturnCode :: integer(), ExecutionState :: #execution_state{}}.
execute_impl(ExecutionState) ->
    {LoginLine, PwdLine} = interact_with_user(),
    Login = string_data_utils:remove_trailing_line_feed(LoginLine),
    PwdString = string_data_utils:remove_trailing_line_feed(PwdLine),
    Pwd = create_pwd_hash(PwdString),
    LoginCommand = #login{login_name = Login, password = Pwd},
    GlobalHandler = ExecutionState#execution_state.global_handler,
    case gen_server:call(GlobalHandler, LoginCommand) of
        #login_success{session_pid = Session, greeting = Greeting} ->
            io:format(Greeting, []),
            NewExecutionState = ExecutionState#execution_state{session = Session},
            {0, NewExecutionState};
        #login_fail{reason = Reason} ->
            io:format(standard_error, "Login's attempt is failed due to the following: ~p~n", [Reason]),
            {255, ExecutionState}
    end.

-spec interact_with_user() -> {LoginLine :: string(), PwdLine :: string()}.
interact_with_user() ->
    OldOptions = io:getopts(),
    OptionsWithoutExpand = lists:keydelete(expand_fun, 1, OldOptions),
    EchoOnOptions = lists:keystore(echo, 1, OptionsWithoutExpand, {echo, true}),
    EchoOffOptions = lists:keystore(echo, 1, OptionsWithoutExpand, {echo, false}),
    io:setopts(EchoOnOptions),
    LoginLine = io:get_line("login:"),
    io:setopts(EchoOffOptions),
    PwdLine = io:get_line("password:"),
    io:setopts(OldOptions),
    {LoginLine, PwdLine}.

-spec create_pwd_hash(PwdString :: string()) -> binary().
create_pwd_hash(PwdString) ->
    crypto_utils:hash(?HASH_TYPE, PwdString, ?HASH_SALT).