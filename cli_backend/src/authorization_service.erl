%% @author std-string

-module(authorization_service).

-behaviour(authorization_service_behaviour).
-behaviour(gen_server).

-include("common_defs.hrl").


%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1, authorize/2]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start(_Config) -> error(not_implemented).

authorize(_User, _CommandName) -> false.

init(_Args) -> error(not_implemented).

handle_call(_request, _From, _State) -> error(not_implemented).

handle_cast(_Request, State) -> {stop, not_supported, State}.

handle_info(_Info, State) -> {stop, not_supported, State}.

terminate(_Reason, _State) -> true.

code_change(_OldVsn, _State, _Extra) -> {error, not_supported}.


%% ====================================================================
%% Internal functions
%% ====================================================================

