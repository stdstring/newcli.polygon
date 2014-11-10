%% @author std-string

-module(frame_item_search_tests).

-include_lib("eunit/include/eunit.hrl").

-include("common_defs.hrl").

-define(PING_MODULE, ping_impl).
-define(CONF_TERM_MODULE, configure_terminal_impl).
-define(LOGIN_MODULE, login_impl).
-define(LOGOUT_MODULE, logout_impl).
-define(INTERFACE_MODULE, interface_impl).
-define(IFRANGE_MODULE, interface_range_impl).
-define(VLAN_MODULE, vlan_impl).
-define(NOVLAN_MODULE, novlan_impl).
-define(SWACCESS_VLAN_MODULE, swaccess_vlan_impl).
-define(NOSWACCESS_VLAN_MODULE, noswaccess_vlan_impl).
-define(NAME_MODULE, name_impl).
-define(NONAME_MODULE, noname_impl).
-define(END_MODULE, end_impl).
-define(EXIT_MODULE, exit_impl).
-define(SHOW_VLAN_MODULE, show_vlan_impl).

-define(FUNCTION, execute).

-define(WORD(Value), #frame_item{type = word, value = Value}).
-define(STRING(Value), #frame_item{type = string, value = Value}).

%% ====================================================================
%% Test functions
%% ====================================================================

search_best_test_() ->
    NameTable = create_name_table(),
    [success_check("search ping XXX", ?PING_MODULE, [?WORD("XXX")], [?WORD("ping"), ?WORD("XXX")], NameTable),
     success_check("search p XXX", ?PING_MODULE, [?WORD("XXX")], [?WORD("p"), ?WORD("XXX")], NameTable),
     fail_check("search png XXX", [?WORD("png"), ?WORD("XXX")], NameTable),
     success_check("search configure terminal", ?CONF_TERM_MODULE, [], [?WORD("configure"), ?WORD("terminal")], NameTable),
     success_check("search c terminal", ?CONF_TERM_MODULE, [], [?WORD("c"), ?WORD("terminal")], NameTable),
     success_check("search c t", ?CONF_TERM_MODULE, [], [?WORD("c"), ?WORD("t")], NameTable),
     success_check("search interface 666", ?INTERFACE_MODULE, [?WORD("666")], [?WORD("interface"), ?WORD("666")], NameTable),
     success_check("search i 666", ?INTERFACE_MODULE, [?WORD("666")], [?WORD("i"), ?WORD("666")], NameTable),
     success_check("search interface range 666", ?IFRANGE_MODULE, [?WORD("666")], [?WORD("interface"), ?WORD("range"), ?WORD("666")], NameTable),
     success_check("search i range 666", ?IFRANGE_MODULE, [?WORD("666")], [?WORD("i"), ?WORD("range"), ?WORD("666")], NameTable),
     success_check("search interface r 666", ?IFRANGE_MODULE, [?WORD("666")], [?WORD("interface"), ?WORD("r"), ?WORD("666")], NameTable),
     success_check("search i r 666", ?IFRANGE_MODULE, [?WORD("666")], [?WORD("i"), ?WORD("r"), ?WORD("666")], NameTable),
     success_check("search name IDDQD", ?NAME_MODULE, [?WORD("IDDQD")], [?WORD("name"), ?WORD("IDDQD")], NameTable),
     success_check("search na IDDQD", ?NAME_MODULE, [?WORD("IDDQD")], [?WORD("na"), ?WORD("IDDQD")], NameTable),
     fail_check("search n IDDQD", [?WORD("n"), ?WORD("IDDQD")], NameTable),
     success_check("search no name", ?NONAME_MODULE, [], [?WORD("no"), ?WORD("name")], NameTable),
     fail_check("search n name", [?WORD("n"), ?WORD("name")], NameTable),
     fail_check("search unknown_command IDDQD", [?WORD("unknown_command"), ?WORD("IDDQD")], NameTable)].

%% ====================================================================
%% Internal functions
%% ====================================================================

create_name_table() ->
    [{[{"ping", 1}], ?PING_MODULE, ?FUNCTION},
     {[{"configure", 1}, {"terminal", 1}], ?CONF_TERM_MODULE, ?FUNCTION},
     {[{"login", 4}], ?LOGIN_MODULE, ?FUNCTION},
     {[{"logout", 4}], ?LOGOUT_MODULE, ?FUNCTION},
     {[{"interface", 1}], ?INTERFACE_MODULE, ?FUNCTION},
     {[{"interface", 1}, {"range", 1}], ?IFRANGE_MODULE, ?FUNCTION},
     {[{"vlan", 1}], ?VLAN_MODULE, ?FUNCTION},
     {[{"no", 2}, {"vlan", 1}], ?NOVLAN_MODULE, ?FUNCTION},
     {[{"switchport", 2}, {"access", 1}, {"vlan", 1}], ?SWACCESS_VLAN_MODULE, ?FUNCTION},
     {[{"no", 2}, {"switchport", 1}, {"access", 1}, {"vlan", 1}], ?NOSWACCESS_VLAN_MODULE, ?FUNCTION},
     {[{"name", 2}], ?NAME_MODULE, ?FUNCTION},
     {[{"no", 2}, {"name", 1}], ?NONAME_MODULE, ?FUNCTION},
     {[{"end", 2}], ?END_MODULE, ?FUNCTION},
     {[{"exit", 2}], ?EXIT_MODULE, ?FUNCTION},
     {[{"show", 2}, {"vlan", 1}], ?SHOW_VLAN_MODULE, ?FUNCTION}].

success_check(Description, Module, Args, Items, NameTable) ->
    {Description, ?_assertEqual({true, Module, ?FUNCTION, Args}, frame_item_search:search_best(Items, NameTable))}.

fail_check(Description, Items, NameTable) ->
    {Description, ?_assertEqual(false, frame_item_search:search_best(Items, NameTable))}.