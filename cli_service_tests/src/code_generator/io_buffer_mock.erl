%% @author std-string

-module(io_buffer_mock).

-export([start/0, send_output/2, send_error/2, get_data/2, reset/1]).

%% ====================================================================
%% API functions
%% ====================================================================

start() ->
    mock_server:execute(io_buffer, start, []).

send_output(Buffer, Message) ->
    mock_server:execute(io_buffer, send_output, [Buffer, Message]).

send_error(Buffer, Message) ->
    mock_server:execute(io_buffer, send_error, [Buffer, Message]).

get_data(Buffer, DataType) ->
    mock_server:execute(io_buffer, get_data, [Buffer, DataType]).

reset(Buffer) ->
    mock_server:execute(io_buffer, reset, [Buffer]).

%% ====================================================================
%% Internal functions
%% ====================================================================