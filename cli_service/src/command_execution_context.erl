%% @author std-string

-module(command_execution_context).

-include("common_defs.hrl").

-define(EXEC_FAILED, "Command execution failed. Return code is ~w\n").

-export([process/2]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec process(Message :: term(), State :: #client_handler_state{}) -> #client_handler_state{}.
process({command_out, Output}, State) ->
    command_helper:send_output(State, Output),
    State;
process({command_err, Error}, State) ->
    command_helper:send_error(State, Error),
    State;
process({command_end, 0}, State) ->
    NewState = State#client_handler_state{},
    process_next_command(NewState);
%%process({command_end, ExecutionState, ReturnCode}, State) ->
%%    Error = message_helper:format(?EXEC_FAILED, [ReturnCode]),
%%    NewState = State#client_handler_state{execution_state = ExecutionState, command_chain = []},
%%    command_helper:send_error(NewState, Error),
%%    command_helper:send_end(NewState),
%%    NewState;
process({command_end, ReturnCode}, State) ->
    Error = message_helper:format(?EXEC_FAILED, [ReturnCode]),
    NewState = State#client_handler_state{},
    command_helper:send_error(NewState, Error),
    command_helper:send_end(NewState),
    NewState;
%%process(interrupt_command, #client_handler_state{current_command = undefined} = State) ->
%%    State;
%%process(interrupt_command, State) ->
%%    Command = State#client_handler_state.current_command,
%%    exit(Command, interrupt),
%%    NewState = State#client_handler_state{command_chain = [], current_command = undefined},
%%    command_helper:send_end(NewState),
%%    NewState.
process(interrupt_command, State) ->
    NewState = State#client_handler_state{},
    command_helper:send_end(NewState),
    NewState.

%% ====================================================================
%% Internal functions
%% ====================================================================

%%-spec process_next_command(State :: #client_handler_state{}) -> #client_handler_state{}.
%%process_next_command(State) ->
%%    case State#client_handler_state.command_chain of
%%        [] ->
%%            command_helper:send_end(State),
%%            State#client_handler_state{command_chain = [], current_command = undefined};
%%        [#command_entry{module = Module, command_line_rest = CommandLineRest} | Rest] ->
%%            ClientHandler = self(),
%%            ExecutionState = State#client_handler_state.execution_state,
%%            Command = apply(Module, execute, [CommandLineRest, ClientHandler, ExecutionState]),
%%            State#client_handler_state{command_chain = Rest, current_command = Command}
%%    end.

-spec process_next_command(State :: #client_handler_state{}) -> #client_handler_state{}.
process_next_command(State) ->
    State.