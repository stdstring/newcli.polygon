%% @author std-string

-module(name_search).

-export([search/2]).

-include("name_search_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

%% Condition = [{Word :: string(), MinPrefixLength :: pos_integer()}]
%% Table = [{Condition, Module :: atom(), Function :: atom()}]
search(_Words, []) -> false;
search(Words, Table) ->
    create_search_result(search_full_match(Words, Table), search_incomplete_match(Words, Table)).

%% ====================================================================
%% Internal functions
%% ====================================================================

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