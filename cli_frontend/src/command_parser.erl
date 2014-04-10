%% @author std-string

-module(command_parser).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([]).

-spec parse(CommandLine :: string(), GlobalConfig :: #global_config{}, ExecutionState :: #execution_state{}) ->
          [{ModuleName :: atom, CommandPid :: pid}] | {'command_parser', Reason :: term()}.
parse(CommandLine, GlobalConfig, ExecutionState) -> [].

%% ====================================================================
%% Internal functions
%% ====================================================================

