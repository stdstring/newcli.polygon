%% @author std-string

-module(exit_module).

-behaviour(command_behaviour).

-export([get_name/0, get_command_body/0, get_help/0, execute/4]).

%% ====================================================================
%% API functions
%% ====================================================================

get_name() -> exit.

get_command_body() -> ["exit"].

get_help() -> "exit help".

execute(_Args, _Stdout, _Stderr, _ExecContext) ->
    throw(enotimpl).

%% ====================================================================
%% Internal functions
%% ====================================================================