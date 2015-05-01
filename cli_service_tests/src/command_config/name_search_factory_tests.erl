%% @author std-string

-module(name_search_factory_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

create_test() ->
    CommandsInfo = create_commands_info(),
    Config = name_search_factory:create(CommandsInfo),
    check(Config, config_terminal_command, [{"configure", 1}, {"terminal", 1}]),
    check(Config, end_command, [{"end", 2}]),
    check(Config, exit_command, [{"exit", 2}]),
    check(Config, interface_command, [{"interface", 1}]),
    check(Config, interface_range_command, [{"interface", 1}, {"range", 1}]),
    check(Config, logout_command, [{"logout", 1}]),
    check(Config, name_command, [{"name", 2}]),
    check(Config, noname_command, [{"no", 2}, {"name", 1}]),
    check(Config, noswitchport_vlan_command, [{"no", 2}, {"switchport", 1}, {"access", 1}, {"vlan", 1}]),
    check(Config, novlan_command, [{"no", 2}, {"vlan", 1}]),
    check(Config, ping_command, [{"ping", 1}]),
    check(Config, show_vlan_command, [{"show", 2}, {"vlan", 1}]),
    check(Config, switchport_vlan_command, [{"switchport", 2}, {"access", 1}, {"vlan", 1}]),
    check(Config, vlan_command, [{"vlan", 1}]),
    ?assertEqual(length(CommandsInfo), length(Config)).

%% ====================================================================
%% Internal functions
%% ====================================================================

create_commands_info() ->
    [{config_terminal_command, ["configure", "terminal"]},
     {end_command, ["end"]},
     {exit_command, ["exit"]},
     {interface_command, ["interface"]},
     {interface_range_command, ["interface", "range"]},
     {logout_command, ["logout"]},
     {name_command, ["name"]},
     {noname_command, ["no", "name"]},
     {noswitchport_vlan_command, ["no", "switchport", "access", "vlan"]},
     {novlan_command, ["no", "vlan"]},
     {ping_command, ["ping"]},
     {show_vlan_command, ["show", "vlan"]},
     {switchport_vlan_command, ["switchport", "access", "vlan"]},
     {vlan_command, ["vlan"]}].

check(Config, Module, Expected) ->
    Result = lists:keyfind(Module, 2, Config),
    ?assertMatch({_Data, Module}, Result),
    {Actual, Module} = Result,
    ?assertEqual(Expected, Actual).
