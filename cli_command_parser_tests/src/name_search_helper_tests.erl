%% @author std-string

-module(name_search_helper_tests).

-include_lib("eunit/include/eunit.hrl").

-include("module_defs.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

search_exact_test_() ->
    NameTable = name_search_config:create(),
    [success_search_exact("search 'ping'", ["ping"], ?PING_MODULE, NameTable),
     success_search_exact("search 'p'", ["p"], ?PING_MODULE, NameTable),
     fail_search_exact("search 'n'", ["n"], NameTable),
     fail_search_exact("search '\"interface range\"'", ["interface range"], NameTable)].

search_suitable_test_() ->
    NameTable = name_search_config:create(),
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

success_search_exact(Description, Words, Expected, NameTable) ->
    {Description, ?_assertEqual({true, Expected}, name_search_helper:search_exact(Words, NameTable))}.

fail_search_exact(Description, Words, NameTable) ->
    {Description, ?_assertEqual(false, name_search_helper:search_exact(Words, NameTable))}.

search_suitable(Description, Words, Expected, NameTable) ->
    {Description, ?_assertEqual(Expected, name_search_helper:search_suitable(Words, NameTable))}.

search_suitable(Description, Words, Prefix, Expected, NameTable) ->
    {Description, ?_assertEqual(Expected, name_search_helper:search_suitable(Words, Prefix, NameTable))}.