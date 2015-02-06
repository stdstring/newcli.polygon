%% @author std-string

-module(frame_item_search).

%%-export([search_best/2, search_suitable/2, search_exact/2]).
-export([search_best/2]).

-include("frame_defs.hrl").
-include("name_search_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec search_best(FrameItems :: [#frame_item{}], NameTable :: name_search_table()) ->
    {'true', Module :: atom(), Rest :: [#frame_item{}]} | 'false'.
search_best(FrameItems, NameTable) ->
    search_best_impl([], FrameItems, undefined, NameTable).

%%-spec search_suitable(FrameItems :: [#frame_item{}], NameTable :: name_search_table()) ->
%%    {ExactModule :: 'undefined' | atom(), Modules :: name_search_table()}.
%%search_suitable(FrameItems, NameTable) ->
%%    case transform_frame_items(FrameItems) of
%%        {true, Values} -> search_suitable_impl(Values, NameTable);
%%        false -> {undefined, []}
%%    end.

%%-spec search_exact(FrameItems :: [#frame_item{}], NameTable :: name_search_table()) ->
%%    {'true', Module :: atom()} | 'false'.
%%search_exact(FrameItems, NameTable) ->
%%    case transform_frame_items(FrameItems) of
%%        {true, Values} -> search_exact_impl(Values, NameTable);
%%        false -> false
%%    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%%-spec transform_frame_items(FrameItems :: [#frame_item{}]) -> {'true', Words :: [string()]} | false.
%%transform_frame_items(FrameItems) ->
%%    transform_frame_items(FrameItems, []).

%%-spec transform_frame_items(FrameItems :: [#frame_item{}], Result :: [string()]) ->
%%     {'true', Words :: [string()]} | false.
%%transform_frame_items([], Result) ->
%%    {true, lists:reverse(Result)};
%%transform_frame_items([#frame_item{type = word, value = Word} | Rest], Result) ->
%%    transform_frame_items(Rest, [Word] ++ Result);
%%transform_frame_items([#frame_item{type = string, value = Word} | Rest], Result) ->
%%    transform_frame_items(Rest, [Word] ++ Result);
%%transform_frame_items([#frame_item{} | _Rest], _Result) ->
%%    false.

%%-spec search_suitable_impl(Values :: [string()], NameTable :: name_search_table()) -> name_search_table().
%%search_suitable_impl(Values, NameTable) ->
%%    case name_search:search(Values, NameTable) of
%%        {true, Module, RowsRest} -> {Module, RowsRest};
%%        {incomplete, RowsRest} -> {undefined, RowsRest};
%%        false -> {undefined, []}
%%    end.

%%-spec search_exact_impl(Values :: [string()], NameTable :: name_search_table()) ->
%%    {'true', Module :: atom()} | 'false'.
%%search_exact_impl(Values, NameTable) ->
%%    case name_search:search(Values, NameTable) of
%%        {true, Module, _RowsRest} -> {true, Module};
%%        _Other -> false
%%    end.

-spec search_best_impl(WordsUsed :: [string()],
                      FrameItems :: [#frame_item{}],
                      SearchResult :: {'true', Module :: atom(), Rest :: [#frame_item{}]} | 'undefined',
                      Rows :: name_search_table()) ->
    {'true', Module :: atom(), Rest :: [#frame_item{}]} | 'false'.
search_best_impl(_WordsUsed, [], undefined, _Rows) -> false;
search_best_impl(_WordsUsed, [], {RecModule, RecRest}, _Rows) -> {true, RecModule, RecRest};
search_best_impl(_WordsUsed, _ItemsRest, undefined, []) -> false;
search_best_impl(_WordsUsed, _ItemsRest, {RecModule, RecRest}, []) -> {true, RecModule, RecRest};
search_best_impl(WordsUsed, [#frame_item{type = word, value = Word} | Rest], undefined, Rows) ->
    search_best_process_word(WordsUsed, Word, Rest, undefined, Rows);
search_best_impl(WordsUsed, [#frame_item{type = word, value = Word} | Rest], {RecModule, RecRest}, Rows) ->
    search_best_process_word(WordsUsed, Word, Rest, {RecModule, RecRest}, Rows);
search_best_impl(WordsUsed, [#frame_item{type = string, value = Word} | Rest], undefined, Rows) ->
    search_best_process_word(WordsUsed, Word, Rest, undefined, Rows);
search_best_impl(WordsUsed, [#frame_item{type = string, value = Word} | Rest], {RecModule, RecRest}, Rows) ->
    search_best_process_word(WordsUsed, Word, Rest, {RecModule, RecRest}, Rows);
search_best_impl(_WordsUsed, [#frame_item{} | _Rest], undefined, _Rows) ->
    false;
search_best_impl(_WordsUsed, [#frame_item{} | _Rest], {RecModule, RecRest}, _Rows) ->
    {true, RecModule, RecRest}.

-spec search_best_process_word(WordsUsed :: [string()],
                               Word :: string(),
                               WordsRest :: [#frame_item{}],
                               SearchResult :: {Module :: atom(), Rest :: [#frame_item{}]} | 'undefined',
                               Rows :: name_search_table()) -> no_return() | 'false'.
search_best_process_word(WordsUsed, Word, WordsRest, undefined, Rows) ->
    NewWordsUsed = WordsUsed ++ [Word],
    case name_search:search(NewWordsUsed, Rows) of
        {true, Module, RowsRest} ->
            NewRecognized = {Module, WordsRest},
            search_best_impl(NewWordsUsed, WordsRest, NewRecognized, RowsRest);
        {incomplete, RowsRest} -> search_best_impl(NewWordsUsed, WordsRest, undefined, RowsRest);
        false -> false
    end;
search_best_process_word(WordsUsed, Word, WordsRest, {RecModule, RecRest}, Rows) ->
    NewWordsUsed = WordsUsed ++ [Word],
    case name_search:search(NewWordsUsed, Rows) of
        {true, Module, RowsRest} ->
            NewRecognized = {Module, WordsRest},
            search_best_impl(NewWordsUsed, WordsRest, NewRecognized, RowsRest);
        {incomplete, RowsRest} -> search_best_impl(NewWordsUsed, WordsRest, {RecModule, RecRest}, RowsRest);
        false -> {true, RecModule, RecRest}
    end.