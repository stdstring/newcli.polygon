%% @author std-string

-module(command_execution_context).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([execute/3]).

-spec execute(CommandLine :: string(), GlobalConfig :: #global_config{}, ExecutionState :: #execution_state{}) -> boolean().
execute(CommandLine, GlobalConfig, ExecutionState) ->
    case command_parser:parse(CommandLine, GlobalConfig, ExecutionState) of
        {command_parser, Reason} -> false;
        Commands -> true
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

