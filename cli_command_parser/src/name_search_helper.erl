%% @author std-string

-module(name_search_helper).

-export([search_exact/2, search_suitable/2, search_suitable/3]).

-include("name_search_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec search_exact(Words :: [string()], Table :: name_search_table()) ->
    {'true', Value :: term()} | 'false'.
search_exact(Words, Table) ->
    case name_search:search(Words, Table) of
        {true, Value, _Rows} -> {true, Value};
        _Other -> false
    end.

-spec search_suitable(Words :: [string()], Table :: name_search_table()) -> [term()].
search_suitable([], Table) ->
    lists:map(fun({_SearchItems, Value}) -> Value end, Table);
search_suitable(Words, Table) ->
    [Prefix | Rest] = lists:reverse(Words),
    search_suitable(lists:reverse(Rest), Prefix, Table).

-spec search_suitable(Words :: [string()], Prefix :: string(), Table :: name_search_table()) -> [term()].
search_suitable(Words, Prefix, Table) ->
    PrefixIndex = length(Words) + 1,
    case name_search:search(Words, Table) of
        {true, _Value, Rows} ->
            process_rows(Prefix, PrefixIndex, Rows);
        {incomplete, Rows} ->
            process_rows(Prefix, PrefixIndex, Rows);
        false -> []
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

process_rows(Prefix, PrefixIndex, Rows) ->
    lists:map(fun({_SearchItems, Value}) -> Value end, filter_by_prefix(Prefix, PrefixIndex, Rows)).

filter_by_prefix(Prefix, PrefixIndex, Rows) ->
    FilterFun = fun({SearchItems, _Value}) ->
        {Word, _MinLength} = lists:nth(PrefixIndex, SearchItems),
        lists:prefix(Prefix, Word)
    end,
    lists:filter(FilterFun, Rows).