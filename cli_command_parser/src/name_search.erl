%% @author std-string

-module(name_search).

-export([search/2]).

-include("name_search_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec search(Words :: [string()], Table :: name_search_table()) ->
    {'true', Value :: term(), Rows :: name_search_table()} | {'incomplete', Rows :: name_search_table()} | 'false'.
search(_Words, []) -> false;
search(Words, Table) ->
    create_search_result(search_full_match(Words, Table), search_incomplete_match(Words, Table)).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec create_search_result(FullMatchResult :: {'true', Value :: term()} | 'false',
                           IncompleteMatchResult :: {'incomplete', Other :: name_search_table()} | 'false') ->
    {'true', Value :: term(), Rows :: name_search_table()} | {'incomplete', Rows :: name_search_table()} | 'false'.
create_search_result(false, false) -> false;
create_search_result({true, Value}, false) -> {true, Value, []};
create_search_result(false, {incomplete, Rows}) -> {incomplete, Rows};
create_search_result({true, Value}, {incomplete, Rows}) -> {true, Value, Rows}.

-spec search_incomplete_match(Words :: [string()], Table :: name_search_table()) ->
    {'incomplete', Other :: name_search_table()} | 'false'.
search_incomplete_match(Words, Table) ->
    Result = lists:filter(fun({Cond, _Value}) -> compare_words(Words, Cond) == incomplete end, Table),
    case Result of
        [] -> false;
        Other -> {incomplete, Other}
    end.

-spec search_full_match(Words :: [string()], Table :: name_search_table()) ->
    {'true', Value :: term()} | 'false'.
search_full_match(Words, Table) ->
    Result = lists:filter(fun({Cond, _Value}) -> compare_words(Words, Cond) == true end, Table),
    case Result of
        [] -> false;
        [{_Cond, Value}] -> {true, Value}
    end.

-spec compare_words(Words :: [string()], SearchItems :: [name_search_item()]) ->
    'true' | 'false' | 'incomplete'.
compare_words([], []) -> true;
compare_words(_WordsRest, []) -> false;
compare_words([], _CondRest) -> incomplete;
compare_words([LeftWord | WordsRest], [{RightWord, MinPrefixLength} | CondRest]) ->
    CompareResult = lists:prefix(LeftWord, RightWord) andalso length(LeftWord) >= MinPrefixLength,
    case CompareResult of
        true -> compare_words(WordsRest, CondRest);
        false -> false
    end.