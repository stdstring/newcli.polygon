%% @author std-string

-module(command_execution_context).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([execute/3]).

-spec execute(CommandLine :: string(), GlobalConfig :: #global_config{}, ExecutionState :: #execution_state{}) -> ExecutionState :: #execution_state{}.
execute(CommandLine, GlobalConfig, ExecutionState) ->
    case command_parser:parse(CommandLine, GlobalConfig, ExecutionState) of
        {command_parser, Reason} ->
            io:format(standard_error, "Command's parsing is failed due to the following: ~p~n", [Reason]),
            ExecutionState;
        Commands ->
            {_Result, NewExecutionState} = execute_impl(Commands, GlobalConfig, ExecutionState),
            NewExecutionState
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec execute_impl(Commands :: [{ModuleName :: atom(), CommandLineRest :: string()}],
                   GlobalConfig :: #global_config{},
                   ExecutionState :: #execution_state{}) -> {ReturnCode :: integer(), ExecutionState :: #execution_state{}}.
execute_impl([], _GlobalConfig, ExecutionState) -> {0, ExecutionState};
execute_impl([{Module, CommandLineRest} | Rest], GlobalConfig, ExecutionState) ->
    {Result, NewExecutionState} = apply(Module, execute, [CommandLineRest, ExecutionState]),
    if
        Result == 0 -> execute_impl(Rest, GlobalConfig, NewExecutionState);
        Result /= 0 ->
            io:format(standard_error, "Command execution failed. Return code is ~p~n", [Result]),
            {Result, NewExecutionState}
    end.