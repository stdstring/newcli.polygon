%% @author std-string

-module(name_searcher).

-export([search_best/2, search/3]).

%% ====================================================================
%% API functions
%% ====================================================================

search_best(_Words, _SearchTable) -> ok.

%% SearchRow = [{Word :: string(), MinPrefixLength :: pos_integer()}]
%% SearchTable = [{SearchRow, Module :: atom(), Function :: atom()}]
search(_Words, _HasMore, []) -> {false, not_found};
search(Words, true, SearchTable) ->
    create_search_result(search_full_match(Words, SearchTable), search_incomplete_match(Words, SearchTable));
search(Words, false, SearchTable) ->
    create_search_result(search_full_match(Words, SearchTable), {false, not_found}).

%% ====================================================================
%% Internal functions
%% ====================================================================

create_search_result({false, not_found}, {false, not_found}) -> {false, not_found};
create_search_result({true, Module, Func}, {false, not_found}) -> {true, Module, Func, []};
create_search_result({false, not_found}, {incomplete, Rows}) -> {incomplete, Rows};
create_search_result({true, Module, Func}, {incomplete, Rows}) -> {true, Module, Func, Rows}.

search_incomplete_match(Words, SearchTable) ->
    Result = lists:filter(fun({Row, _Module, _Func}) -> compare_words(Words, Row) == incomplete end, SearchTable),
    case Result of
        [] -> {false, not_found};
        Other -> {incomplete, Other}
    end.

search_full_match(Words, SearchTable) ->
    Result = lists:filter(fun({Row, _Module, _Func}) -> compare_words(Words, Row) == true end, SearchTable),
    case Result of
        [] -> {false, not_found};
        [{_SearchRow, Module, Func}] -> {true, Module, Func}
    end.

compare_words([], []) -> true;
compare_words(_WordsRest, []) -> false;
compare_words([], _SearchRowRest) -> incomplete;
compare_words([LeftWord | WordsRest], [{RightWord, MinPrefixLength} | SearchRowRest]) ->
    CompareResult = lists:prefix(LeftWord, RightWord) andalso length(LeftWord) >= MinPrefixLength,
    case CompareResult of
        true -> compare_words(WordsRest, SearchRowRest);
        false -> false
    end.