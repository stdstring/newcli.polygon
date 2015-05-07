%% @author std-string

-module(command_helper).

-include("authentication_defs.hrl").
-include("command_defs.hrl").
-include("common_defs.hrl").

-export([send_output/2, send_error/2, send_end/2]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec send_output(State :: #client_handler_state{}, Output :: string()) -> 'ok'.
send_output(State, Output) when is_record(State, client_handler_state) ->
    Endpoint = State#client_handler_state.endpoint,
    cli_terminal_endpoint:handle_output(Endpoint, Output).

-spec send_error(State :: #client_handler_state{}, Error :: string()) -> 'ok'.
send_error(State, Error) when is_record(State, client_handler_state) ->
    Endpoint = State#client_handler_state.endpoint,
    cli_terminal_endpoint:handle_error(Endpoint, Error).

-spec send_end(State :: #client_handler_state{}, ExecutionState :: 'ex_stop' | 'ex_continue') -> 'ok'.
send_end(State, ?EX_CONTINUE) when is_record(State, client_handler_state) ->
    %%ExecutionState = State#client_handler_state.execution_state,
    Endpoint = State#client_handler_state.endpoint,
    Prompt = prompt_factory:generate_prompt(State),
    cli_terminal_endpoint:handle_end(Endpoint, Prompt, ?EX_CONTINUE);
send_end(State, ?EX_STOP) when is_record(State, client_handler_state) ->
    %%ExecutionState = State#client_handler_state.execution_state,
    Endpoint = State#client_handler_state.endpoint,
    Prompt = prompt_factory:generate_prompt(State),
    cli_terminal_endpoint:handle_end(Endpoint, Prompt, ?EX_STOP).

%% ====================================================================
%% Internal functions
%% ====================================================================