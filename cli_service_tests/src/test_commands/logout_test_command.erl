%% @author std-string

-module(logout_test_command).

-behaviour(command_behaviour).

-export([get_name/0, get_command_body/0, get_help/0, execute/4]).

%% ====================================================================
%% API functions
%% ====================================================================

get_name() -> logout.

get_command_body() -> ["logout"].

get_help() -> "logout help".

execute(_Args, _Stdout, _Stderr, _ExecContext) ->
    throw(enotimpl).

%% ====================================================================
%% Internal functions
%% ====================================================================