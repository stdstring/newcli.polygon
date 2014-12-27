%% @author std-string

-module(command_helper).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([send_output/2, send_error/2, send_end/1]).

-spec send_output(State :: #client_handler_state{}, Output :: string()) -> 'ok'.
send_output(State, Output) when is_record(State, client_handler_state) ->
    Endpoint = State#client_handler_state.endpoint,
    cli_terminal_endpoint:handle_output(Endpoint, Output).

-spec send_error(State :: #client_handler_state{}, Error :: string()) -> 'ok'.
send_error(State, Error) when is_record(State, client_handler_state) ->
    Endpoint = State#client_handler_state.endpoint,
    cli_terminal_endpoint:handle_error(Endpoint, Error).

-spec send_end(State :: #client_handler_state{}) -> 'ok'.
send_end(State) when is_record(State, client_handler_state) ->
    %%ExecutionState = State#client_handler_state.execution_state,
    Endpoint = State#client_handler_state.endpoint,
    %%Prompt = prompt_factory:generate_prompt(ExecutionState),
    Prompt = "",
    cli_terminal_endpoint:handle_end(Endpoint, Prompt).


%% ====================================================================
%% Internal functions
%% ====================================================================