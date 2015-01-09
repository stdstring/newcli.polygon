%% @author stdstring

-module(name_search_config_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

create_test() ->
    CommandsInfo = create_commands_info(),
    Config = name_search_config:create(CommandsInfo),
    check(Config, config_terminal_test_command, [{"configure", 1}, {"terminal", 1}]),
    check(Config, end_test_command, [{"end", 2}]),
    check(Config, exit_test_command, [{"exit", 2}]),
    check(Config, interface_test_command, [{"interface", 1}]),
    check(Config, interface_range_test_command, [{"interface", 1}, {"range", 1}]),
    check(Config, login_test_command, [{"login", 4}]),
    check(Config, logout_test_command, [{"logout", 4}]),
    check(Config, name_test_command, [{"name", 2}]),
    check(Config, noname_test_command, [{"no", 2}, {"name", 1}]),
    check(Config, noswitchport_vlan_test_command, [{"no", 2}, {"switchport", 1}, {"access", 1}, {"vlan", 1}]),
    check(Config, novlan_test_command, [{"no", 2}, {"vlan", 1}]),
    check(Config, ping_test_command, [{"ping", 1}]),
    check(Config, show_vlan_test_command, [{"show", 2}, {"vlan", 1}]),
    check(Config, switchport_vlan_test_command, [{"switchport", 2}, {"access", 1}, {"vlan", 1}]),
    check(Config, vlan_test_command, [{"vlan", 1}]),
    ?assertEqual(length(CommandsInfo), length(Config)).

%% ====================================================================
%% Internal functions
%% ====================================================================

create_commands_info() ->
    [{config_terminal, config_terminal_test_command},
     {'end', end_test_command},
     {exit, exit_test_command},
     {interface, interface_test_command},
     {interface_range, interface_range_test_command},
     {login, login_test_command},
     {logout, logout_test_command},
     {name, name_test_command},
     {noname, noname_test_command},
     {noswitchport_vlan, noswitchport_vlan_test_command},
     {novlan, novlan_test_command},
     {ping, ping_test_command},
     {show_vlan, show_vlan_test_command},
     {switchport_vlan, switchport_vlan_test_command},
     {vlan, vlan_test_command}].

check(Config, Module, Expected) ->
    Result = lists:keyfind(Module, 2, Config),
    ?assertMatch({_Data, Module}, Result),
    {Actual, Module} = Result,
    ?assertEqual(Expected, Actual).
