%% @author std-string

-module(login_command).

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
execute(_CommandLineRest, ExecutionState) -> {0, ExecutionState}.

%% ====================================================================
%% Internal functions
%% ====================================================================

