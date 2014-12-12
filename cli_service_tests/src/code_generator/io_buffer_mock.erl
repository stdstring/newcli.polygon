%% @author std-string

-module(io_buffer_mock).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([start/1, send_output/2, send_error/2, get_data/2, reset/1]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(io_buffer_state, {data = [] :: [{'output', Message :: string()} | {'error', Message :: string()}],
                          trace = [] :: [tuple()]}).

%% ====================================================================
%% API functions
%% ====================================================================

start(Data) ->
    gen_server:start_link(?MODULE, Data, []).

send_output(_Buffer, _Message) ->
    ?assert(false).

send_error(_Buffer, _Message) ->
    ?assert(false).

get_data(Buffer, DataType) ->
    gen_server:call(Buffer, {get_data, DataType}).

reset(_Buffer) ->
    ?assert(false).

init(Data) ->
    {ok, #io_buffer_state{data = Data}}.

handle_call({get_data, both}, _From, State) ->
    case State#io_buffer_state.trace of
        [] -> {reply, State#io_buffer_state.data, #io_buffer_state{data = [], trace = [{get_data, both}]}};
        _Other -> ?assert(false)
    end;
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