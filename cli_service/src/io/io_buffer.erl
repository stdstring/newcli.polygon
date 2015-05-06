%% @author std-string

-module(io_buffer).

-include("io_buffer_defs.hrl").

-export([start/1, send_output/2, send_error/2, get_data/2, reset/1]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(IoBufferModule :: atom()) -> {'ok', Pid :: pid()} | {'error', Reason :: term()}.
start(IoBufferModule) ->
    gen_server:start_link(IoBufferModule, [], []).

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

%% ====================================================================
%% Internal functions
%% ====================================================================