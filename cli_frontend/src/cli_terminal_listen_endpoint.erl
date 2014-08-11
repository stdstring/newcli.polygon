%% @author std-string

-module(cli_terminal_listen_endpoint).

%% -record(listen_state, {listen_socket = undefined :: 'undefined' | socket()}).
-record(listen_state, {listen_socket = undefined :: 'undefined' | term()}).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start_link/1]).
-export([init/1]).

start_link(PortNumber) ->
    proc_lib:start_link(cli_terminal_listen_endpoint, init, [PortNumber]).

init(PortNumber) ->
    case gen_tcp:listen(PortNumber, [binary, {packet, 4}, {active, once}]) of
        {ok, ListenSocket} ->
            proc_lib:init_ack({ok, self()}),
            process_listen(#listen_state{listen_socket = ListenSocket});
        {error, Reason} ->
            proc_lib:init_ack({error, {listen_error, Reason}})
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

process_listen(State) ->
    case gen_tcp:accept(State#listen_state.listen_socket) of
        {ok, _Socket} ->
            %% create cli_terminal_endpoint
            ok;
        {error, _Reason} ->
            %% some logging
            ok
    end,
    process_listen(State).