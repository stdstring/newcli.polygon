%% @author std-string

-module(noswitchport_vlan_test_command).

-behaviour(command_behaviour).

-export([get_name/0, get_command_body/0, get_help/0, execute/4]).

%% ====================================================================
%% API functions
%% ====================================================================

get_name() -> noswitchport_vlan.

get_command_body() -> ["no", "switchport", "access", "vlan"].

get_help() -> "no switchport access vlan help".

execute(_Args, _Stdout, _Stderr, _ExecContext) ->
    throw(enotimpl).

%% ====================================================================
%% Internal functions
%% ====================================================================