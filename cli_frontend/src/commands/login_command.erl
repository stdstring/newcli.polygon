%% @author std-string

-module(login_command).

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
    OldOptions = io:getopts(),
    OptionsWithoutExpand = lists:keydelete(expand_fun, 1, OldOptions),
    EchoOnOptions = lists:keystore(echo, 1, OptionsWithoutExpand, {echo, true}),
    EchoOffOptions = lists:keystore(echo, 1, OptionsWithoutExpand, {echo, false}),
    io:setopts(EchoOnOptions),
    LoginLine = io:get_line("login:"),
    io:setopts(EchoOffOptions),
    PwdLine = io:get_line("password:"),
    io:setopts(OldOptions),
    Login = string:strip(LoginLine, right, $\n),
    PwdString = string:strip(PwdLine, right, $\n),
    Pwd = <<>>,
    LoginCommand = #login{login_name = Login, password = Pwd},
    case gen_server:call(global_input_endpoint, LoginCommand) of
        #login_success{session_pid = Session, greeting = Greeting} ->
            io:format(Greeting, []),
            NewExecutionState = ExecutionState#execution_state{session = Session},
            {0, NewExecutionState};
        #login_fail{reason = Reason} ->
            io:format(standard_error, "Login's attempt is failed due to the following: ~p~n", [Reason]),
            {255, ExecutionState}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

