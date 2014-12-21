%% @author std-string

-module(cli_terminal_listen_endpoint).

-include("common_defs.hrl").

%% -record(listen_state, {listen_socket = undefined :: 'undefined' | socket()}).
-record(listen_state, {listen_socket = undefined :: 'undefined' | term()}).

-export([start/1]).
%% proc_lib export
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(Config :: #cli_terminal_config{}) -> {'ok', Pid :: pid()}.
start(Config) ->
    proc_lib:start_link(?MODULE, init, [Config]).

init(#cli_terminal_config{port_number = PortNumber}) ->
    case gen_tcp:listen(PortNumber, [binary, {packet, 4}, {active, once}]) of
        {ok, ListenSocket} ->
            register(?LISTEN_ENDPOINT_NAME, self()),
            proc_lib:init_ack({ok, self()}),
            process_listen(#listen_state{listen_socket = ListenSocket});
        {error, Reason} ->
            proc_lib:init_ack({error, {listen_error, Reason}})
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_listen(State :: #listen_state{}) -> no_return().
process_listen(State) ->
    case gen_tcp:accept(State#listen_state.listen_socket) of
        {ok, Socket} ->
            create_endpoint(Socket);
        {error, _Reason} ->
            %% some logging
            ok
    end,
    process_listen(State).

%%-spec create_endpoint(Socket :: socket()) -> 'ok'.
-spec create_endpoint(Socket :: term()) -> 'ok'.
create_endpoint(Socket) ->
    case cli_terminal_supervisor:create_endpoint(Socket) of
        {ok, Endpoint} ->
            gen_tcp:controlling_process(Socket, Endpoint);
        {error, Reason} ->
            gen_tcp:close(Socket),
            error({create_endpoint, Reason})
    end.