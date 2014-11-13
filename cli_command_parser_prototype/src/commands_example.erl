-module(commands_example).

-export([ping_exec/1,
         configure_terminal_exec/1,
         login_exec/1,
         logout_exec/1,
         interface_exec/1,
         interface_range_exec/1,
         vlan_exec/1,
         novlan_exec/1,
         swaccess_vlan_exec/1,
         noswaccess_vlan_exec/1,
         name_exec/1,
         noname_exec/1,
         end_exec/1,
         exit_exec/1,
         show_vlan_exec/1]).

%% ====================================================================
%% API functions
%% ====================================================================

ping_exec(Args) ->
    "ping" ++ create_arg_string(Args).

configure_terminal_exec(Args) ->
    "configure terminal" ++ create_arg_string(Args).

login_exec(Args) ->
    "login" ++ create_arg_string(Args).

logout_exec(Args) ->
    "logout" ++ create_arg_string(Args).

interface_exec(Args) ->
    "interface" ++ create_arg_string(Args).

interface_range_exec(Args) ->
    "interface range" ++ create_arg_string(Args).

vlan_exec(Args) ->
    "vlan" ++ create_arg_string(Args).

novlan_exec(Args) ->
    "no vlan" ++ create_arg_string(Args).

swaccess_vlan_exec(Args) ->
    "switchport access vlan" ++ create_arg_string(Args).

noswaccess_vlan_exec(Args) ->
    "no switchport access vlan" ++ create_arg_string(Args).

name_exec(Args) ->
    "name" ++ create_arg_string(Args).

noname_exec(Args) ->
    "no name" ++ create_arg_string(Args).

end_exec(Args) ->
    "end" ++ create_arg_string(Args).

exit_exec(Args) ->
    "exit" ++ create_arg_string(Args).

show_vlan_exec(Args) ->
    "show vlan" ++ create_arg_string(Args).

%% ====================================================================
%% Internal functions
%% ====================================================================

create_arg_string(Args) ->
    create_arg_string(Args, "").

create_arg_string([], Result) -> Result;
create_arg_string([Arg | Args], "") ->
    NewResult = lists:flatten(io_lib:format(" ~p", [Arg])),
    create_arg_string(Args, NewResult);
create_arg_string([Arg | Args], Result) ->
    NewResult = Result ++ lists:flatten(io_lib:format(", ~p", [Arg])),
    create_arg_string(Args, NewResult).