%% @author std-string

%% TODO (std_string) : think about more robust solution
-module(io_buffer).

-behaviour(gen_server).

-include("io_buffer_defs.hrl").

-export([start/0, send_output/2, send_error/2, get_data/2, reset/1]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(io_buffer_state, {data = [] :: [#output{} | #error{}]}).

%% ====================================================================
%% API functions
%% ====================================================================

-spec start() -> {'ok', Pid :: pid()} | {'error', Reason :: term()}.
start() ->
    gen_server:start_link(?MODULE, [], []).

-spec send_output(Buffer :: pid(), Message :: string()) -> 'ok'.
send_output(Buffer, Message) ->
    gen_server:call(Buffer, #output{message = Message}).

-spec send_error(Buffer :: pid(), Message :: string()) -> 'ok'.
send_error(Buffer, Message) ->
    gen_server:call(Buffer, #error{message = Message}).

-spec get_data(Buffer :: pid(), DataType :: 'output' | 'error' | 'both') ->
    [{'output', Message :: string()} | {'error', Message :: string()}].
get_data(Buffer, DataType) ->
    gen_server:call(Buffer, #get_data{data_type = DataType}).

-spec reset(Buffer :: pid()) -> 'ok'.
reset(Buffer) ->
    gen_server:call(Buffer, #reset{}).

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