%% @author std-string

-module(io_buffer_mock).

-behaviour(gen_server).

-export([start/1, get_data/2]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================

start(_Data) ->
    ok.

get_data(_Buffer, _DataType) ->
    [].

%% ====================================================================
%% Internal functions
%% ====================================================================