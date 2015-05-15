%% @author std-string

-module(command_executor).

-include("authentication_defs.hrl").
-include("client_handler_defs.hrl").
-include("command_defs.hrl").
-include("common_defs.hrl").

-export([process/2]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec process(Message :: term(), State :: #client_handler_state{}) -> #client_handler_state{}.
process({?COMMAND_OUTPUT, Output}, State) ->
    command_helper:send_output(State, Output),
    State;
process({?COMMAND_ERROR, Error}, State) ->
    command_helper:send_error(State, Error),
    State;
process({?FINISH_COMMAND, _ReturnCode, _ExecutionContext}, State) ->
    State;
process({?FINISH_EXEC, _ReturnCode, ExecutionContext}, State) ->
    User = list_utils:get_value_by_key_with_default(ExecutionContext, ?USER_KEY, 1, undefined),
    ExecutionState = list_utils:get_value_by_key_with_default(ExecutionContext, ?EX_STATE_KEY, 1, ?EX_CONTINUE),
    clear_after_command(State),
    IntermediateState = State#client_handler_state{user = User, current_command = undefined},
    command_helper:send_end(IntermediateState, ExecutionState),
    client_downtime_timer:start(IntermediateState);
process(?INTERRUPT, #client_handler_state{current_command = undefined} = State) ->
    client_downtime_timer:restart(State);
process(?INTERRUPT, State) ->
    Command = State#client_handler_state.current_command,
    exit(Command, interrupt),
    clear_after_command(State),
    IntermediateState = State#client_handler_state{current_command = undefined},
    command_helper:send_end(IntermediateState, ?EX_CONTINUE),
    client_downtime_timer:start_timer(IntermediateState).

%% ====================================================================
%% Internal functions
%% ====================================================================

clear_after_command(#client_handler_state{command_module = CommandModule}) ->
    code:delete(CommandModule),
    code:purge(CommandModule).