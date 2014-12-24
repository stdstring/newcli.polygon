%% @author std-string

-module(syntax_analyzer_config).

-export([create/1]).

-include("frame_defs.hrl").
-include("lexical_defs.hrl").
-include("name_search_defs.hrl").
-include("syntax_defs.hrl").
-include("token_defs.hrl").
-include("syntax_analyzer_defs.hrl").

-define(WORD_TEMPLATE, ?WORD_TOKEN(undefined)).
-define(STRING_TEMPLATE, ?STRING_TOKEN(undefined)).

-define(COMMAND_ACTION, fun command_action/3).
-define(ARGS_ACTION, fun args_action/3).

%% ====================================================================
%% API functions
%% ====================================================================

-spec create(NameTable :: name_search_table()) -> #syntax_analyzer_config{}.
create(NameTable) ->
    Table =[{{?COMMAND, ?WORD_TEMPLATE}, {[?WORD_TERM, ?ARGS], ?COMMAND_ACTION}},
            {{?ARGS, ?WORD_TEMPLATE}, {[?WORD_TERM, ?ARGS], ?ARGS_ACTION}},
            {{?ARGS, ?STRING_TEMPLATE}, {[?STRING_TERM, ?ARGS], ?ARGS_ACTION}},
            {{?ARGS, ?END_TOKEN}, {[?END_TERM], ?ARGS_ACTION}}],
    #syntax_analyzer_config{syntax_table =  dict:from_list(Table), start_symbol = ?COMMAND, production_config = NameTable}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec command_action(NameTable :: term(), State :: 'undefined' | term(), Token :: #token{}) ->
    {'true', State :: term()} | {'false', Reason :: term()}.
command_action(_NameTable, undefined, ?WORD_TOKEN(Word)) ->
    CommandFrame = #command_frame{items = [#frame_item{type = word, value = Word}]},
    {true, CommandFrame}.

-spec args_action(NameTable :: term(), State :: term(), Token :: #token{}) ->
    {'true', State :: term()} | {'false', Reason :: term()}.
args_action(_NameTable, #command_frame{items = Items}, ?WORD_TOKEN(Word)) ->
    NewCommandFrame = #command_frame{items = [#frame_item{type = word, value = Word}] ++ Items},
    {true, NewCommandFrame};
args_action(_NameTable, #command_frame{items = Items}, ?STRING_TOKEN(String)) ->
    NewCommandFrame = #command_frame{items = [#frame_item{type = string, value = String}] ++ Items},
    {true, NewCommandFrame};
args_action(NameTable, #command_frame{items = Items}, ?END_TOKEN) ->
    case generate_result(lists:reverse(Items), NameTable) of
        {true, Module, Args} -> {true, {Module, Args}};
        false -> {false, command_not_found}
    end.

-spec generate_result(Items :: [#frame_item{}], NameTable :: name_search_table()) ->
    {'true', Module :: atom(), Args :: [term()]} | 'false'.
generate_result(Items, NameTable) ->
    case frame_item_search:search_best(Items, NameTable) of
        {true, Module, RestItems} ->
            Args = lists:map(fun(#frame_item{value = Value}) -> Value end, RestItems),
            {true, Module, Args};
        false -> false
    end.