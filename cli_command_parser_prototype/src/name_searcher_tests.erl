%% @author std-string

-module(name_searcher_tests).

-include_lib("eunit/include/eunit.hrl").

-export([conf_terminal_impl/1,
         end_impl/1,
         exit_impl/1,
         interface_impl/1,
         irange_impl/1,
         login_impl/1,
         logout_impl/1,
         name_impl/1,
         noname_impl/1,
         noswaccess_vlan_impl/1,
         novlan_impl/1,
         ping_impl/1,
         show_vlan_impl/1,
         swaccess_vlan_impl/1,
         vlan_impl/1]).

%% ====================================================================
%% Test functions
%% ====================================================================

search_test() ->
    NameTable = create_name_table(),
    io:format(user, "~p~n", [name_searcher:search_best(["ping", "XXX"], NameTable)]).

%% ====================================================================
%% API functions
%% ====================================================================

conf_terminal_impl(Args) ->
    io:format(user, "configure terminal command~n", []),
    show_args(Args).

end_impl(Args) ->
    io:format(user, "end command~n", []),
    show_args(Args).

exit_impl(Args) ->
    io:format(user, "exit command~n", []),
    show_args(Args).

interface_impl(Args) ->
    io:format(user, "interface command~n", []),
    show_args(Args).

irange_impl(Args) ->
    io:format(user, "interface range command~n", []),
    show_args(Args).

login_impl(Args) ->
    io:format(user, "login command~n", []),
    show_args(Args).

logout_impl(Args) ->
    io:format(user, "logout command~n", []),
    show_args(Args).

name_impl(Args) ->
    io:format(user, "name command~n", []),
    show_args(Args).

noname_impl(Args) ->
    io:format(user, "no name command~n", []),
    show_args(Args).

noswaccess_vlan_impl(Args) ->
    io:format(user, "no switchport access vlan command~n", []),
    show_args(Args).

novlan_impl(Args) ->
    io:format(user, "no vlan command~n", []),
    show_args(Args).

ping_impl(Args) ->
    io:format(user, "ping command~n", []),
    show_args(Args).

show_vlan_impl(Args) ->
    io:format(user, "show vlan command~n", []),
    show_args(Args).

swaccess_vlan_impl(Args) ->
    io:format(user, "switchport access vlan command~n", []),
    show_args(Args).

vlan_impl(Args) ->
    io:format(user, "vlan command~n", []),
    show_args(Args).

%% ====================================================================
%% Internal functions
%% ====================================================================

create_name_table() ->
    [{[{"ping", 1}], ?MODULE, ping_impl},
     {[{"configure", 1}, {"terminal", 1}], ?MODULE, conf_terminal_impl},
     {[{"login", 4}], ?MODULE, login_impl},
     {[{"logout", 4}], ?MODULE, logout_impl},
     {[{"interface", 1}], ?MODULE, interface_impl},
     {[{"interface", 1}, {"range", 1}], ?MODULE, irange_impl},
     {[{"vlan", 1}], ?MODULE, vlan_impl},
     {[{"no", 2}, {"vlan", 1}], ?MODULE, novlan_impl},
     {[{"switchport", 2}, {"access", 1}, {"vlan", 1}], ?MODULE, swaccess_vlan_impl},
     {[{"no", 2}, {"switchport", 1}, {"access", 1}, {"vlan", 1}], ?MODULE, noswaccess_vlan_impl},
     {[{"name", 2}], ?MODULE, name_impl},
     {[{"no", 2}, {"name", 1}], ?MODULE, noname_impl},
     {[{"end", 2}], ?MODULE, end_impl},
     {[{"exit", 2}], ?MODULE, exit_impl},
     {[{"show", 2}, {"vlan", 1}], ?MODULE, show_vlan_impl}].

show_args(Args) ->
    io:format(user, "arguments: ~p~n", [Args]).