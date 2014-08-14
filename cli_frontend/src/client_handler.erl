-module(client_handler).

-behaviour(gen_server).

-include("common_defs.hrl").

-record(client_handler_state, {config :: #global_config{}, state :: #execution_state{}}).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1, get_current_state/1, get_extensions/2]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

get_current_state(Handler) -> ok.

get_extensions(Handler, CommandLine) -> ok.

init(_Args) -> {ok, #client_handler_state{}}.

handle_call(_Request, _From, State) -> {stop, enotsup, State}.

handle_cast(_Request, State) -> {stop, enotsup, State}.

handle_info(_Info, State) -> {stop, enotsup, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================