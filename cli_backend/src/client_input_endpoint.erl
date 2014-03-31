%% @author std-string

-module(client_input_endpoint).

-behaviour(gen_server).

-include("message_defs.hrl").
-include("common_defs.hrl").

-record(client_state, {global_config = #global_config{} :: #global_config{}, client_config = #client_config{} :: #client_config{}}).

%% ====================================================================
%% API functions
%% ====================================================================

-export([]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

init({GlobalConfig, }) -> {ok, #client_state{}}.

handle_call(#command{message = CommandLine}, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(#logout{}, State) -> {noreply, State}.

handle_info(_Info, State) -> {stop, not_supported, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

