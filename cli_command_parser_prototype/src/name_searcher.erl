%% @author std-string

-module(name_searcher).

-export([search_best/2, search/2]).

%% ====================================================================
%% API functions
%% ====================================================================

search_best(Words, Table) ->
    search_best_impl([], Words, undefined, Table).

%% Condition = [{Word :: string(), MinPrefixLength :: pos_integer()}]
%% Table = [{Condition, Module :: atom(), Function :: atom()}]
search(_Words, []) -> false;
search(Words, Table) ->
    create_search_result(search_full_match(Words, Table), search_incomplete_match(Words, Table)).

%% ====================================================================
%% Internal functions
%% ====================================================================

search_best_impl(_WordsUsed, [], undefined, _Rows) -> false;
search_best_impl(_WordsUsed, [], {Module, Func, Rest}, _Rows) -> {true, Module, Func, Rest};
search_best_impl(_WordsUsed, _WordsRest, undefined, []) -> false;
search_best_impl(_WordsUsed, _WordsRest, {Module, Func, Rest}, []) -> {true, Module, Func, Rest};
search_best_impl(WordsUsed, [Word | Rest], Recognized, Rows) ->
    NewWordsUsed = WordsUsed ++ [Word],
    case search(NewWordsUsed, Rows) of
        {true, Module, Func, RowsRest} ->
            NewRecognized = {Module, Func, RowsRest},
            search_best_impl(NewWordsUsed, Rest, NewRecognized, RowsRest);
        {incomplete, RowsRest} -> search_best_impl(NewWordsUsed, Rest, Recognized, RowsRest);
        false -> false
    end.

create_search_result(false, false) -> false;
create_search_result({true, Module, Func}, false) -> {true, Module, Func, []};
create_search_result(false, {incomplete, Rows}) -> {incomplete, Rows};
create_search_result({true, Module, Func}, {incomplete, Rows}) -> {true, Module, Func, Rows}.

search_incomplete_match(Words, Table) ->
    Result = lists:filter(fun({Cond, _Module, _Func}) -> compare_words(Words, Cond) == incomplete end, Table),
    case Result of
        [] -> false;
        Other -> {incomplete, Other}
    end.

search_full_match(Words, Table) ->
    Result = lists:filter(fun({Cond, _Module, _Func}) -> compare_words(Words, Cond) == true end, Table),
    case Result of
        [] -> false;
        [{_Cond, Module, Func}] -> {true, Module, Func}
    end.

compare_words([], []) -> true;
compare_words(_WordsRest, []) -> false;
compare_words([], _CondRest) -> incomplete;
compare_words([LeftWord | WordsRest], [{RightWord, MinPrefixLength} | CondRest]) ->
    CompareResult = lists:prefix(LeftWord, RightWord) andalso length(LeftWord) >= MinPrefixLength,
    case CompareResult of
        true -> compare_words(WordsRest, CondRest);
        false -> false
    end.