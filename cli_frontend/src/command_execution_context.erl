%% @author std-string

-module(command_execution_context).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([execute/3]).

-spec execute(CommandLine :: string(), GlobalConfig :: #global_config{}, ExecutionState :: #execution_state{}) ->
          {boolean(), ExecutionState :: #execution_state{}}.
execute(CommandLine, GlobalConfig, ExecutionState) ->
    case command_parser:parse(CommandLine, GlobalConfig, ExecutionState) of
        {command_parser, Reason} ->
            {false, ExecutionState};
        Commands ->
            {Result, NewExecutionState} = execute_impl(Commands, GlobalConfig, ExecutionState),
            {Result == 0, NewExecutionState}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec execute_impl(Commands :: [{ModuleName :: atom(), CommandPid :: pid()}],
                   GlobalConfig :: #global_config{},
                   ExecutionState :: #execution_state{}) -> {ReturnCode :: integer(), ExecutionState :: #execution_state{}}.
execute_impl([], _GlobalConfig, ExecutionState) -> {0, ExecutionState};
execute_impl([{Module, Pid} | Rest], GlobalConfig, ExecutionState) ->
    {Result, NewExecutionState} = apply(Module, execute, [Pid, ExecutionState]),
    if
        Result == 0 -> execute_impl(Rest, GlobalConfig, NewExecutionState);
        Result /= 0 -> {Result, NewExecutionState}
    end.