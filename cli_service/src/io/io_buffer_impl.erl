%% @author std-string

-module(io_buffer_impl).

-behaviour(gen_server).

-include("io_buffer_defs.hrl").

%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(io_buffer_state, {data = [] :: [#output{} | #error{}]}).

%% ====================================================================
%% API functions
%% ====================================================================

init(_Args) -> {ok, #io_buffer_state{}}.

handle_call(#output{} = Message, _From, #io_buffer_state{data = Data}) ->
    {reply, ok, #io_buffer_state{data = [Message] ++ Data}};
handle_call(#error{} = Message, _From, #io_buffer_state{data = Data}) ->
    {reply, ok, #io_buffer_state{data = [Message] ++ Data}};
handle_call(#get_data{data_type = DataType}, _From, #io_buffer_state{data = Data} = State) ->
    case DataType of
        output ->
            {reply, lists:reverse(lists:filter(fun(Message) -> is_record(Message, output) end, Data)), State};
        error ->
            {reply, lists:reverse(lists:filter(fun(Message) -> is_record(Message, error) end, Data)), State};
        both -> {reply, lists:reverse(Data), State}
    end;
handle_call(#reset{}, _From, _State) ->
    {reply, ok, #io_buffer_state{}};
handle_call(_Request, _From, State) ->
    {stop, enotsup, State}.

handle_cast(_Request, State) -> {stop, enotsup, State}.

handle_info(_Other, State) -> {stop, enotsup, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================