%% @author std-string

-module(name_search_helper_tests).

-include_lib("eunit/include/eunit.hrl").

-include("module_defs.hrl").
-include("name_search_defs.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

search_exact_test_() ->
    NameTable = create_config(),
    [success_search_exact("search 'ping'", ["ping"], ?PING_MODULE, NameTable),
     success_search_exact("search 'p'", ["p"], ?PING_MODULE, NameTable),
     fail_search_exact("search 'n'", ["n"], NameTable),
     fail_search_exact("search '\"interface range\"'", ["interface range"], NameTable)].

search_suitable_test_() ->
    NameTable = create_config(),
    [search_suitable("search ''", [], ?ALL_MODULES, NameTable),
     search_suitable("search 'p'", ["p"], [?PING_MODULE], NameTable),
     search_suitable("search 'p'", [], "p", [?PING_MODULE], NameTable),
     search_suitable("search 'i'", ["i"], [?INTERFACE_MODULE, ?IFRANGE_MODULE], NameTable),
     search_suitable("search 'i'", [], "i", [?INTERFACE_MODULE, ?IFRANGE_MODULE], NameTable),
     search_suitable("search 'i r'", ["i", "r"], [?IFRANGE_MODULE], NameTable),
     search_suitable("search 'i'", ["i"], "r", [?IFRANGE_MODULE], NameTable),
     search_suitable("search 'n'", ["n"], [?NOVLAN_MODULE, ?NOSWACCESS_VLAN_MODULE, ?NAME_MODULE, ?NONAME_MODULE], NameTable),
     search_suitable("search 'n'", [], "n", [?NOVLAN_MODULE, ?NOSWACCESS_VLAN_MODULE, ?NAME_MODULE, ?NONAME_MODULE], NameTable)].

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec create_config() -> name_search_table().
create_config() ->
    [{[{"ping", 1}], ?PING_MODULE},
     {[{"configure", 1}, {"terminal", 1}], ?CONF_TERM_MODULE},
     {[{"login", 4}], ?LOGIN_MODULE},
     {[{"logout", 4}], ?LOGOUT_MODULE},
     {[{"interface", 1}], ?INTERFACE_MODULE},
     {[{"interface", 1}, {"range", 1}], ?IFRANGE_MODULE},
     {[{"vlan", 1}], ?VLAN_MODULE},
     {[{"no", 2}, {"vlan", 1}], ?NOVLAN_MODULE},
     {[{"switchport", 2}, {"access", 1}, {"vlan", 1}], ?SWACCESS_VLAN_MODULE},
     {[{"no", 2}, {"switchport", 1}, {"access", 1}, {"vlan", 1}], ?NOSWACCESS_VLAN_MODULE},
     {[{"name", 2}], ?NAME_MODULE},
     {[{"no", 2}, {"name", 1}], ?NONAME_MODULE},
     {[{"end", 2}], ?END_MODULE},
     {[{"exit", 2}], ?EXIT_MODULE},
     {[{"show", 2}, {"vlan", 1}], ?SHOW_VLAN_MODULE}].

success_search_exact(Description, Words, Expected, NameTable) ->
    {Description, ?_assertEqual({true, Expected}, name_search_helper:search_exact(Words, NameTable))}.

fail_search_exact(Description, Words, NameTable) ->
    {Description, ?_assertEqual(false, name_search_helper:search_exact(Words, NameTable))}.

search_suitable(Description, Words, Expected, NameTable) ->
    {Description, ?_assertEqual(Expected, name_search_helper:search_suitable(Words, NameTable))}.

search_suitable(Description, Words, Prefix, Expected, NameTable) ->
    {Description, ?_assertEqual(Expected, name_search_helper:search_suitable(Words, Prefix, NameTable))}.