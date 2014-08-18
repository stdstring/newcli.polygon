%% @author std-string

-module(command_execution_context).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

%%-export([execute/3]).
-export([process/2]).

-spec process(Message :: term(), State :: #client_handler_state{}) -> #client_handler_state{}.
process({command_out, _Output}, State) -> State;
process({command_err, _Error}, State) -> State;
process({command_end, _ExecutionState, _ReturnCode}, State) -> State;
process(interrupt_command, State) -> State.

%%-spec execute(CommandLine :: string(), GlobalConfig :: #global_config{}, ExecutionState :: #execution_state{}) -> ExecutionState :: #execution_state{}.
%%execute("", _GlobalConfig, ExecutionState) -> ExecutionState;
%%execute(CommandLine, GlobalConfig, ExecutionState) ->
%%    case command_parser:parse(CommandLine, GlobalConfig) of
%%        {command_parser, Reason} ->
%%            io:format(standard_error, "Command's parsing is failed due to the following: ~w~n", [Reason]),
%%            ExecutionState;
%%        Commands ->
%%            {_Result, NewExecutionState} = execute_impl(Commands, GlobalConfig, ExecutionState),
%%            NewExecutionState
%%    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%%-spec execute_impl(Commands :: [{ModuleName :: atom(), CommandLineRest :: string()}],
%%                   GlobalConfig :: #global_config{},
%%                   ExecutionState :: #execution_state{}) -> {ReturnCode :: integer(), ExecutionState :: #execution_state{}}.
%%execute_impl([], _GlobalConfig, ExecutionState) -> {0, ExecutionState};
%%execute_impl([{Module, CommandLineRest} | Rest], GlobalConfig, ExecutionState) ->
%%    {Result, NewExecutionState} = apply(Module, execute, [CommandLineRest, ExecutionState]),
%%    if
%%        Result == 0 -> execute_impl(Rest, GlobalConfig, NewExecutionState);
%%        Result /= 0 ->
%%            io:format(standard_error, "Command execution failed. Return code is ~w~n", [Result]),
%%            {Result, NewExecutionState}
%%    end.