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
    [success_check("search ping XXX", ?PING_MODULE, [?WORD("XXX")], [?WORD("ping"), ?WORD("XXX")], NameTable),
     success_check("search p XXX", ?PING_MODULE, [?WORD("XXX")], [?WORD("p"), ?WORD("XXX")], NameTable),
     fail_check("search png XXX", [?WORD("png"), ?WORD("XXX")], NameTable),
     success_check("search ping \"XXX\"", ?PING_MODULE, [?STRING("XXX")], [?WORD("ping"), ?STRING("XXX")], NameTable),
     fail_check("search \"ping\" XXX", [?STRING("ping"), ?WORD("XXX")], NameTable),
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
     fail_check("search unknown_command IDDQD", [?WORD("unknown_command"), ?WORD("IDDQD")], NameTable),
     fail_check("search 666 IDDQD", [?FRAME_ITEM(integer, 666), ?WORD("IDDQD")], NameTable)].

%% ====================================================================
%% Internal functions
%% ====================================================================

success_check(Description, Module, Args, Items, NameTable) ->
    {Description, ?_assertEqual({true, Module, Args}, frame_item_search:search_best(Items, NameTable))}.

fail_check(Description, Items, NameTable) ->
    {Description, ?_assertEqual(false, frame_item_search:search_best(Items, NameTable))}.