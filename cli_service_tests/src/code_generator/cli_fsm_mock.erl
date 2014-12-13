%% @author std-string

-module(cli_fsm_mock).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([start/0, process_command/2, get_current_state/1]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================

start() ->
    ?assert(false).

process_command(_FsmPid, _CommandName) ->
    ?assert(false).

get_current_state(_FsmPid) ->
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