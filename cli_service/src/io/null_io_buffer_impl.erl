%% @author std-string

-module(null_io_buffer_impl).

-behaviour(gen_server).

-include("io_buffer_defs.hrl").

%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(null_io_buffer_state, {}).

%% ====================================================================
%% API functions
%% ====================================================================

init(_Args) -> {ok, #null_io_buffer_state{}}.

handle_call(#output{}, _From, State) ->
    {reply, ok, State};
handle_call(#error{}, _From, State) ->
    {reply, ok, State};
handle_call(#get_data{}, _From, State) ->
    {reply, [], State};
handle_call(#reset{}, _From, _State) ->
    {reply, ok, #null_io_buffer_state{}};
handle_call(_Request, _From, State) ->
    {stop, enotsup, State}.

handle_cast(_Request, State) -> {stop, enotsup, State}.

handle_info(_Other, State) -> {stop, enotsup, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================