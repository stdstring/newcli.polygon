%% @author std-string

-module(name_search_tests).

-include_lib("eunit/include/eunit.hrl").

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

%% ====================================================================
%% Test functions
%% ====================================================================

search_test_() ->
    NameTable = create_name_table(),
    [success_check("search ping", ["ping"], ?PING_MODULE, 0, NameTable),
     success_check("search p", ["p"], ?PING_MODULE, 0, NameTable),
     fail_check("search ping XXX", ["ping", "XXX"], NameTable),
     success_check("search interface", ["interface"], ?INTERFACE_MODULE, 1, NameTable),
     success_check("search i", ["i"], ?INTERFACE_MODULE, 1, NameTable),
     success_check("search interface range", ["interface", "range"], ?IFRANGE_MODULE, 0, NameTable),
     success_check("search i range", ["i", "range"], ?IFRANGE_MODULE, 0, NameTable),
     success_check("search interface r", ["interface", "r"], ?IFRANGE_MODULE, 0, NameTable),
     success_check("search i r", ["i", "r"], ?IFRANGE_MODULE, 0, NameTable),
     success_check("search i r", ["i", "r"], ?IFRANGE_MODULE, 0, NameTable),
     incomplete_check("search no", ["no"], 3, NameTable),
     fail_check("search n", ["n"], NameTable),
     fail_check("search unknown_command", ["unknown_command"], NameTable)].

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

success_check(Description, Words, Module, RowsCount, NameTable) ->
    {Description, ?_assert(success_check_impl(Words, Module, RowsCount, NameTable))}.

success_check_impl(Words, Module, RowsCount, NameTable) ->
    {true, Module, ?FUNCTION, Rows} = name_search:search(Words, NameTable),
    length(Rows) == RowsCount.

incomplete_check(Description, Words, RowsCount, NameTable) ->
    {Description, ?_assert(incomplete_check_impl(Words, RowsCount, NameTable))}.

incomplete_check_impl(Words, RowsCount, NameTable) ->
    {incomplete, Rows} = name_search:search(Words, NameTable),
    length(Rows) == RowsCount.

fail_check(Description, Words, NameTable) ->
    {Description, ?_assertNot(name_search:search(Words, NameTable))}.