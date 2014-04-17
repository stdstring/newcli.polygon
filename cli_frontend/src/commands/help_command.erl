%% @author std-string

-module(help_command).

%%-behaviour(gen_client).

-include("common_defs.hrl").

-record(state, {}).

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_name/0, get_command_body/0, create/1, execute/2]).

-spec get_name() -> atom().
get_name() -> help_command.

-spec get_command_body() -> [string()].
get_command_body() -> ["?"].

-spec create(CommandLineRest :: string()) -> {'ok', Command :: pid()} | {'error', Reason :: term()}.
create(CommandLineRest) -> {error, not_implemented}.

-spec execute(Command :: pid(), ExecutionState :: #execution_state{}) -> {ReturnCode :: integer(), ExecutionState :: #execution_state{}}.
execute(Command, ExecutionState) -> {0, ExecutionState}.


%% ====================================================================
%% Internal functions
%% ====================================================================

