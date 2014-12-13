%% @author std-string

-module(client_handler_mock).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([start/0, process_command/2, interrupt_command/1, get_current_state/1, get_extensions/2, exit/1, send_output/2, send_error/2, finish_command/2]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================

start() ->
    ?assert(false).

process_command(_Handler, _CommandLine) ->
    ?assert(false).

interrupt_command(_Handler) ->
    ?assert(false).

get_current_state(_Handler) ->
    ?assert(false).

get_extensions(_Handler, _CommandLine) ->
    ?assert(false).

send_output(_Handler, _Output) ->
    ?assert(false).

send_error(_Handler, _Error) ->
    ?assert(false).

finish_command(_Handler, _ReturnCode) ->
    ?assert(false).

exit(_Handler) ->
    ?assert(false).

init(_Args) ->
    ?assert(false).

handle_call(_Request, _From, _State) ->
    ?assert(false).

handle_cast(_Request, _State) ->
    ?assert(false).

handle_info(_Info, _State) ->
    ?assert(false).

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================