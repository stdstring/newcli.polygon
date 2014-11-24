%% @author std-string

-module(frame_item_search).

-export([search_best/2]).

-include("frame_defs.hrl").
-include("name_search_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec search_best(FrameItems :: [#frame_item{}], NameTable :: name_search_table()) ->
    {'true', Module :: atom(), Function :: atom(), Rest :: [#frame_item{}]} | 'false'.
search_best(FrameItems, NameTable) ->
    search_best_impl([], FrameItems, undefined, NameTable).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec search_best_impl(WordsUsed :: [string()],
                      FrameItems :: [#frame_item{}],
                      SearchResult :: {'true', Module :: atom(), Function :: atom(), Rest :: [#frame_item{}]} | 'undefined',
                      Rows :: name_search_table()) ->
    {'true', Module :: atom(), Function :: atom(), Rest :: [#frame_item{}]} | 'false'.
search_best_impl(_WordsUsed, [], undefined, _Rows) -> false;
search_best_impl(_WordsUsed, [], {RecModule, RecFunc, RecRest}, _Rows) -> {true, RecModule, RecFunc, RecRest};
search_best_impl(_WordsUsed, _ItemsRest, undefined, []) -> false;
search_best_impl(_WordsUsed, _ItemsRest, {RecModule, RecFunc, RecRest}, []) -> {true, RecModule, RecFunc, RecRest};
search_best_impl(WordsUsed, [#frame_item{type = word, value= Word} | Rest], undefined, Rows) ->
    NewWordsUsed = WordsUsed ++ [Word],
    case name_search:search(NewWordsUsed, Rows) of
        {true, Module, Func, RowsRest} ->
            NewRecognized = {Module, Func, Rest},
            search_best_impl(NewWordsUsed, Rest, NewRecognized, RowsRest);
        {incomplete, RowsRest} -> search_best_impl(NewWordsUsed, Rest, undefined, RowsRest);
        false -> false
    end;
search_best_impl(WordsUsed, [#frame_item{type = word, value= Word} | Rest], {RecModule, RecFunc, RecRest}, Rows) ->
    NewWordsUsed = WordsUsed ++ [Word],
    case name_search:search(NewWordsUsed, Rows) of
        {true, Module, Func, RowsRest} ->
            NewRecognized = {Module, Func, Rest},
            search_best_impl(NewWordsUsed, Rest, NewRecognized, RowsRest);
        {incomplete, RowsRest} -> search_best_impl(NewWordsUsed, Rest, {RecModule, RecFunc, RecRest}, RowsRest);
        false -> {true, RecModule, RecFunc, RecRest}
    end;
search_best_impl(_WordsUsed, [#frame_item{} | _Rest], undefined, _Rows) ->
    false;
search_best_impl(_WordsUsed, [#frame_item{} | _Rest], {RecModule, RecFunc, RecRest}, _Rows) ->
    {true, RecModule, RecFunc, RecRest}.
