%% @author std-string

-module(show_vlan_module).

-behaviour(command_behaviour).

-export([get_name/0, get_command_body/0, get_help/0, execute/4]).

%% ====================================================================
%% API functions
%% ====================================================================

get_name() -> show_vlan.

get_command_body() -> ["show", "vlan"].

get_help() -> "show vlan help".

execute(_Args, _Stdout, _Stderr, _ExecContext) ->
    throw(enotimpl).

%% ====================================================================
%% Internal functions
%% ====================================================================