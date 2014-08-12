%% @author std-string

-module(cli_terminal_endpoint).

-behaviour(gen_server).

-include("cli_terminal_common_defs.hrl").
-include("cli_terminal_message_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start(_TerminalState) -> ok.

init(_State) -> ok.

handle_call(_Request, _From, State) -> {stop, enotsup, State}.

handle_cast(_Request, State) -> {stop, enotsup, State}.

handle_info({tcp, Socket, Data}, State) ->
    Request = binary_to_term(Data),
    Result = process_request(Request, Socket),
    process_response(Result, Socket),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State)->
    {stop, {shutdown, socket_closed}, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

process_request(#command{command_line = _CommandLine}, _Socket) -> ok;
process_request(#interrupt{}, _Socket) ->
    %% we won't do anything now
    no_response;
process_request(#current_state_request{}, Socket) ->
    %% some action
    Prompt = "...",
    process_response(#current_state_response{prompt = Prompt}, Socket);
process_request(#extension_request{command_line = _CommandLine}, Socket) ->
    %% some action
    ExtensionList = [],
    process_response(#extension_response{extension_list = ExtensionList}, Socket);
process_request(#exit{}, _Socket) ->
    %% some action
    no_response.

process_response(no_response, _Socket) -> ok;
process_response(#command_out{} = Response, Socket) ->
    gen_tcp:send(Socket, term_to_binary(Response));
process_response(#command_err{} = Response, Socket) ->
    gen_tcp:send(Socket, term_to_binary(Response));
process_response(#'end'{} = Response, Socket) ->
    gen_tcp:send(Socket, term_to_binary(Response));
process_response(#error{} = Response, Socket) ->
    gen_tcp:send(Socket, term_to_binary(Response));
process_response(#current_state_response{} = Response, Socket) ->
    gen_tcp:send(Socket, term_to_binary(Response));
process_response(#extension_response{} = Response, Socket) ->
    gen_tcp:send(Socket, term_to_binary(Response)).