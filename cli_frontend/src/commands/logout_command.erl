%% @author std-string

-module(logout_command).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_name/0, get_command_body/0, execute/2]).

-spec get_name() -> atom().
get_name() -> logout_command.

-spec get_command_body() -> [string()].
get_command_body() -> ["logout"].

%%-spec create(CommandLineRest :: string()) -> {'ok', Command :: pid()} | {'error', Reason :: term()}.
%%create(CommandLineRest) -> {error, not_implemented}.

-spec execute(CommandLineRest :: string(), ExecutionState :: #execution_state{}) -> {ReturnCode :: integer(), ExecutionState :: #execution_state{}}.
execute("", ExecutionState) ->
    {ReturnCode, NewExecutionState} = backend_command:execute("logout", ExecutionState),
    io:format("You are logged out.~n", []),
    {ReturnCode, NewExecutionState#execution_state{session = undefined}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

