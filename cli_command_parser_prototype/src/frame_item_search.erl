%% @author std-string

-module(frame_item_search).

-export([search_best/2]).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

search_best(FrameItems, NameTable) ->
    search_best_impl([], FrameItems, undefined, NameTable).

%% ====================================================================
%% Internal functions
%% ====================================================================

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
