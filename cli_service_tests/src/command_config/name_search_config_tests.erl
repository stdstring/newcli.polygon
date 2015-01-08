%% @author stdstring

-module(name_search_config_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

create_test() ->
    Config = name_search_config:create(create_commands_info()),
    check(Config, config_terminal_module, [{"configure", 1}, {"terminal", 1}]),
    check(Config, end_module, [{"end", 2}]),
    check(Config, exit_module, [{"exit", 2}]),
    check(Config, interface_module, [{"interface", 1}]),
    check(Config, interface_range_module, [{"interface", 1}, {"range", 1}]),
    check(Config, login_module, [{"login", 4}]),
    check(Config, logout_module, [{"logout", 4}]),
    check(Config, name_module, [{"name", 2}]),
    check(Config, noname_module, [{"no", 2}, {"name", 1}]),
    check(Config, noswitchport_vlan_module, [{"no", 2}, {"switchport", 1}, {"access", 1}, {"vlan", 1}]),
    check(Config, novlan_module, [{"no", 2}, {"vlan", 1}]),
    check(Config, ping_module, [{"ping", 1}]),
    check(Config, show_vlan_module, [{"show", 2}, {"vlan", 1}]),
    check(Config, switchport_vlan_module, [{"switchport", 2}, {"access", 1}, {"vlan", 1}]),
    check(Config, vlan_module, [{"vlan", 1}]).

%% ====================================================================
%% Internal functions
%% ====================================================================

create_commands_info() ->
    [{config_terminal, config_terminal_module},
     {'end', end_module},
     {exit, exit_module},
     {interface, interface_module},
     {interface_range, interface_range_module},
     {login, login_module},
     {logout, logout_module},
     {name, name_module},
     {noname, noname_module},
     {noswitchport_vlan, noswitchport_vlan_module},
     {novlan, novlan_module},
     {ping, ping_module},
     {show_vlan, show_vlan_module},
     {switchport_vlan, switchport_vlan_module},
     {vlan, vlan_module}].

check(Config, Module, Expected) ->
    Result = lists:keyfind(Module, 2, Config),
    ?assertMatch({_Data, Module}, Result),
    {Actual, Module} = Result,
    ?assertEqual(Expected, Actual).
