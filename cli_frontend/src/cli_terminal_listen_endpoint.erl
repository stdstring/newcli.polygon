%% @author std-string

-module(cli_terminal_listen_endpoint).

-behaviour(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================

%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

init(_State) -> ok.

handle_call(_Request, _From, State) -> {stop, enotsup, State}.

handle_cast(_Request, State) -> {stop, enotsup, State}.

handle_info(_Info, State) -> {stop, enotsup, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
