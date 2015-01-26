%% @author std-string

-module(frame_item_search_tests).

-include_lib("eunit/include/eunit.hrl").

-include("module_defs.hrl").
-include("frame_defs.hrl").

-define(WORD(Value), #frame_item{type = word, value = Value}).
-define(STRING(Value), #frame_item{type = string, value = Value}).
-define(FRAME_ITEM(Type, Value), #frame_item{type = Type, value = Value}).

%% ====================================================================
%% Test functions
%% ====================================================================

search_best_test_() ->
    NameTable = name_search_config:create(),
    [success_search_best_check("search ping XXX", ?PING_MODULE, [?WORD("XXX")], [?WORD("ping"), ?WORD("XXX")], NameTable),
     success_search_best_check("search p XXX", ?PING_MODULE, [?WORD("XXX")], [?WORD("p"), ?WORD("XXX")], NameTable),
     fail_search_best_check("search png XXX", [?WORD("png"), ?WORD("XXX")], NameTable),
     success_search_best_check("search ping \"XXX\"", ?PING_MODULE, [?STRING("XXX")], [?WORD("ping"), ?STRING("XXX")], NameTable),
     success_search_best_check("search \"ping\" XXX", ?PING_MODULE, [?WORD("XXX")], [?STRING("ping"), ?WORD("XXX")], NameTable),
     fail_search_best_check("search \"ping XXX\"", [?STRING("ping XXX")], NameTable),
     success_search_best_check("search configure terminal", ?CONF_TERM_MODULE, [], [?WORD("configure"), ?WORD("terminal")], NameTable),
     success_search_best_check("search c terminal", ?CONF_TERM_MODULE, [], [?WORD("c"), ?WORD("terminal")], NameTable),
     success_search_best_check("search c t", ?CONF_TERM_MODULE, [], [?WORD("c"), ?WORD("t")], NameTable),
     success_search_best_check("search interface 666", ?INTERFACE_MODULE, [?WORD("666")], [?WORD("interface"), ?WORD("666")], NameTable),
     success_search_best_check("search i 666", ?INTERFACE_MODULE, [?WORD("666")], [?WORD("i"), ?WORD("666")], NameTable),
     success_search_best_check("search interface range 666", ?IFRANGE_MODULE, [?WORD("666")], [?WORD("interface"), ?WORD("range"), ?WORD("666")], NameTable),
     success_search_best_check("search i range 666", ?IFRANGE_MODULE, [?WORD("666")], [?WORD("i"), ?WORD("range"), ?WORD("666")], NameTable),
     success_search_best_check("search interface r 666", ?IFRANGE_MODULE, [?WORD("666")], [?WORD("interface"), ?WORD("r"), ?WORD("666")], NameTable),
     success_search_best_check("search i r 666", ?IFRANGE_MODULE, [?WORD("666")], [?WORD("i"), ?WORD("r"), ?WORD("666")], NameTable),
     success_search_best_check("search name IDDQD", ?NAME_MODULE, [?WORD("IDDQD")], [?WORD("name"), ?WORD("IDDQD")], NameTable),
     success_search_best_check("search na IDDQD", ?NAME_MODULE, [?WORD("IDDQD")], [?WORD("na"), ?WORD("IDDQD")], NameTable),
     fail_search_best_check("search n IDDQD", [?WORD("n"), ?WORD("IDDQD")], NameTable),
     success_search_best_check("search no name", ?NONAME_MODULE, [], [?WORD("no"), ?WORD("name")], NameTable),
     fail_search_best_check("search n name", [?WORD("n"), ?WORD("name")], NameTable),
     fail_search_best_check("search unknown_command IDDQD", [?WORD("unknown_command"), ?WORD("IDDQD")], NameTable),
     fail_search_best_check("search 666 IDDQD", [?FRAME_ITEM(integer, 666), ?WORD("IDDQD")], NameTable),
     fail_search_best_check("empty search", [], NameTable)].

search_suitable_test_() ->
    NameTable = name_search_config:create(),
    [search_suitable_check("search ping", [?PING_MODULE], [?WORD("ping")], NameTable),
     search_suitable_check("search p", [?PING_MODULE], [?WORD("p")], NameTable),
     search_suitable_check("search pong", [], [?WORD("pong")], NameTable),
     search_suitable_check("search interface", [?INTERFACE_MODULE, ?IFRANGE_MODULE], [?WORD("interface")], NameTable),
     search_suitable_check("search i", [?INTERFACE_MODULE, ?IFRANGE_MODULE], [?WORD("i")], NameTable),
     search_suitable_check("search interface range", [?IFRANGE_MODULE], [?WORD("interface"), ?WORD("range")], NameTable),
     search_suitable_check("search interface r", [?IFRANGE_MODULE], [?WORD("interface"), ?WORD("r")], NameTable),
     search_suitable_check("search no", [?NOVLAN_MODULE, ?NOSWACCESS_VLAN_MODULE, ?NONAME_MODULE], [?WORD("no")], NameTable),
     search_suitable_check("search \"ping\"", [?PING_MODULE], [?STRING("ping")], NameTable),
     search_suitable_check("search ping with other type", [], [?FRAME_ITEM(some_type, "ping")], NameTable)].

search_exact_test_() ->
    NameTable = name_search_config:create(),
    [success_search_exact_check("search ping", ?PING_MODULE, [?WORD("ping")], NameTable),
     success_search_exact_check("search p", ?PING_MODULE, [?WORD("p")], NameTable),
     success_search_exact_check("search interface", ?INTERFACE_MODULE, [?WORD("interface")], NameTable),
     success_search_exact_check("search i", ?INTERFACE_MODULE, [?WORD("i")], NameTable),
     fail_search_exact_check("search pong", [?WORD("pong")], NameTable),
     fail_search_exact_check("search no", [?WORD("no")], NameTable),
     success_search_exact_check("search \"ping\"", ?PING_MODULE, [?STRING("ping")], NameTable),
     fail_search_exact_check("search ping with other type", [?FRAME_ITEM(some_type, "ping")], NameTable)].

%% ====================================================================
%% Internal functions
%% ====================================================================

success_search_best_check(Description, Module, Args, Items, NameTable) ->
    {Description, ?_assertEqual({true, Module, Args}, frame_item_search:search_best(Items, NameTable))}.

fail_search_best_check(Description, Items, NameTable) ->
    {Description, ?_assertEqual(false, frame_item_search:search_best(Items, NameTable))}.

search_suitable_check(Description, Expected, Items, NameTable) ->
    {Description, ?_assertEqual(Expected, frame_item_search:search_suitable(Items, NameTable))}.

success_search_exact_check(Description, Module, Items, NameTable) ->
    {Description, ?_assertEqual({true, Module}, frame_item_search:search_exact(Items, NameTable))}.

fail_search_exact_check(Description, Items, NameTable) ->
    {Description, ?_assertEqual(false, frame_item_search:search_exact(Items, NameTable))}.