%% @author std-string

-module(cli_terminal_endpoint).

-behaviour(gen_server).

-include("cli_terminal_message_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

init(_State) -> ok.

handle_call(_Request, _From, State) -> {stop, enotsup, State}.

handle_cast(_Request, State) -> {stop, enotsup, State}.

handle_info({tcp, Socket, Data}, State) ->
    Request = binary_to_term(Data),
    process_request(Request),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State)->
    {stop, {shutdown, socket_closed}, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

process_request(#command{command_line = _CommandLine}) -> ok;
process_request(#'end'{}) -> ok;
process_request(#interrupt{}) -> ok;
process_request(#current_state_request{}) -> ok;
process_request(#extension_request{command_line = _CommandLine}) -> ok.