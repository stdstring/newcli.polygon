%% @author std-string

-module(end_module).

-behaviour(command_behaviour).

-export([get_name/0, get_command_body/0, get_help/0, execute/4]).

%% ====================================================================
%% API functions
%% ====================================================================

get_name() -> 'end'.

get_command_body() -> ["end"].

get_help() -> "end help".

execute(_Args, _Stdout, _Stderr, _ExecContext) ->
    throw(enotimpl).

%% ====================================================================
%% Internal functions
%% ====================================================================