%% @author std-string

-module(name_search_tests).

-include_lib("eunit/include/eunit.hrl").

-include("function_defs.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

search_test_() ->
    NameTable = name_search_config:create(),
    [success_check("search ping", ["ping"], ?PING_FUNCTION, 0, NameTable),
     success_check("search p", ["p"], ?PING_FUNCTION, 0, NameTable),
     fail_check("search ping XXX", ["ping", "XXX"], NameTable),
     success_check("search interface", ["interface"], ?INTERFACE_FUNCTION, 1, NameTable),
     success_check("search i", ["i"], ?INTERFACE_FUNCTION, 1, NameTable),
     success_check("search interface range", ["interface", "range"], ?IFRANGE_FUNCTION, 0, NameTable),
     success_check("search i range", ["i", "range"], ?IFRANGE_FUNCTION, 0, NameTable),
     success_check("search interface r", ["interface", "r"], ?IFRANGE_FUNCTION, 0, NameTable),
     success_check("search i r", ["i", "r"], ?IFRANGE_FUNCTION, 0, NameTable),
     success_check("search i r", ["i", "r"], ?IFRANGE_FUNCTION, 0, NameTable),
     incomplete_check("search no", ["no"], 3, NameTable),
     fail_check("search n", ["n"], NameTable),
     fail_check("search unknown_command", ["unknown_command"], NameTable)].

%% ====================================================================
%% Internal functions
%% ====================================================================

success_check(Description, Words, Function, RowsCount, NameTable) ->
    {Description, ?_assert(success_check_impl(Words, Function, RowsCount, NameTable))}.

success_check_impl(Words, Function, RowsCount, NameTable) ->
    {true, ?MODULE_NAME, Function, Rows} = name_search:search(Words, NameTable),
    length(Rows) == RowsCount.

incomplete_check(Description, Words, RowsCount, NameTable) ->
    {Description, ?_assert(incomplete_check_impl(Words, RowsCount, NameTable))}.

incomplete_check_impl(Words, RowsCount, NameTable) ->
    {incomplete, Rows} = name_search:search(Words, NameTable),
    length(Rows) == RowsCount.

fail_check(Description, Words, NameTable) ->
    {Description, ?_assertNot(name_search:search(Words, NameTable))}.