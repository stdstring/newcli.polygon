%% @author std-string

-module(logout_command).

%%-behaviour(gen_client).

-record(state, {}).

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_name/0, get_command_body/0, create/1, execute/2]).

-spec get_name() -> atom().
get_name() -> logout_command.

-spec get_command_body() -> [string()].
get_command_body() -> ["logout"].

-spec create(CommandLineRest :: string()) -> {'ok', Command :: pid()} | {'error', Reason :: term()}.
create(CommandLineRest) -> {error, not_implemented}.

-spec execute(Command :: pid(), ExecutionState :: #execution_state{}) -> {ReturnCode :: integer(), ExecutionState :: #execution_state{}}.
execute(Command, ExecutionState) -> {0, ExecutionState}.

%% ====================================================================
%% Internal functions
%% ====================================================================

