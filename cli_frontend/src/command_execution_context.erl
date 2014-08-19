%% @author std-string

-module(command_execution_context).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([process/2]).

-spec process(Message :: term(), State :: #client_handler_state{}) -> #client_handler_state{}.
process({command_out, Output}, State) ->
    Endpoint = State#client_handler_state.endpoint,
    cli_terminal_endpoint:handle_output(Endpoint, Output),
    State;
process({command_err, Error}, State) ->
    Endpoint = State#client_handler_state.endpoint,
    cli_terminal_endpoint:handle_error(Endpoint, Error),
    State;
process({command_end, ExecutionState, 0}, State) ->
    NewState = State#client_handler_state{execution_state = ExecutionState},
    process_next_command(NewState);
process({command_end, ExecutionState, ReturnCode}, State) ->
    Error = lists:flatten(io_lib:format("Command execution failed. Return code is ~w\n", [ReturnCode])),
    Endpoint = State#client_handler_state.endpoint,
    cli_terminal_endpoint:handle_error(Endpoint, Error),
    Prompt = prompt_factory:generate_prompt(ExecutionState),
    cli_terminal_endpoint:handle_end(Endpoint, Prompt),
    State#client_handler_state{execution_state = ExecutionState, command_chain = []};
process(interrupt_command, State) ->
    %% some real action
    State.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_next_command(State :: #client_handler_state{}) -> #client_handler_state{}.
process_next_command(State) ->
    case State#client_handler_state.command_chain of
        [] ->
            Endpoint = State#client_handler_state.endpoint,
            Prompt = prompt_factory:generate_prompt(State#client_handler_state.execution_state),
            cli_terminal_endpoint:handle_end(Endpoint, Prompt),
            State;
        [#command_entry{module = Module, command_line_rest = CommandLineRest} | Rest] ->
            ClientHandler = self(),
            ExecutionState = State#client_handler_state.execution_state,
            apply(Module, execute, [CommandLineRest, ClientHandler, ExecutionState]),
            State#client_handler_state{command_chain = Rest}
    end.