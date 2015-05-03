%% @author std-string

-module(logout_command).

-behaviour(command_behaviour).

-export([get_name/0, get_command_body/0, get_help/0, execute/4]).

-include("authentication_defs.hrl").
-include("command_defs.hrl").

%% TODO (std_string) : move into common place
-define(BAD_ARGS_MESSAGE, "Bad arguments\n").
-define(BAD_ARGS_CODE, 255).
-define(MISSING_USER_MESSAGE, "Missing user\n").
-define(MISSING_USER_CODE, 255).
-define(LOGOUT_TEMPLATE, "User ~p is logged out\n").

%% ====================================================================
%% API functions
%% ====================================================================

get_name() -> logout.

get_command_body() -> ["logout"].

get_help() -> "logout help".

execute(Args, Stdout, Stderr, ExecContext) ->
    case check_args(Args) of
        false -> command_utils:process_error(Stderr, ?BAD_ARGS_MESSAGE, ?BAD_ARGS_CODE, ExecContext);
        true -> process_command(Stdout, Stderr, ExecContext)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec check_args(Args :: [term()]) -> boolean().
check_args([]) -> true;
check_args(_Other) -> false.

-spec process_command(Stdout :: pid(), Stderr :: pid(), ExecContext :: [{Key :: atom(), Value :: term()}]) ->
    {ReturnValue :: integer(), ExecContext :: [{Key :: atom(), Value :: term()}]}.
process_command(Stdout, Stderr, ExecContext) ->
    case lists:keyfind(?USER_KEY, 1, ExecContext) of
        false -> command_utils:process_error(Stderr, ?MISSING_USER_MESSAGE, ?MISSING_USER_CODE, ExecContext);
        {?USER_KEY, User} ->
            IntermediateExecContext = lists:keydelete(?USER_KEY, 1, ExecContext),
            NewExecContext = lists:keystore(?EX_STATE_KEY, 1, IntermediateExecContext, {?EX_STATE_KEY, ?EX_STOP}),
            Message = string_utils:format(?LOGOUT_TEMPLATE, [User#user.username]),
            command_utils:send_output(Stdout, Message),
            {0, NewExecContext}
    end.