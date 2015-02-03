%% @author std-string

-module(syntax_analyzer_config).

-export([create/1]).

-include("frame_defs.hrl").
-include("lexical_defs.hrl").
-include("name_search_defs.hrl").
-include("syntax_defs.hrl").
-include("token_defs.hrl").
-include("syntax_analyzer_defs.hrl").

%% token templates
-define(WORD_TEMPLATE, ?WORD_TOKEN(undefined)).
-define(STRING_TEMPLATE, ?STRING_TOKEN(undefined)).
%% action defs
-define(COMMAND_ACTION, fun command_action/3).
-define(ARGS_ACTION, fun args_action/3).
%% help command def
-define(HELP_COMMAND_CHAR, $?).
-define(HELP_COMMAND, [?HELP_COMMAND_CHAR]).

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
    process_word(#command_frame{}, Word).

-spec args_action(NameTable :: term(), State :: term(), Token :: #token{}) ->
    {'true', State :: term()} | {'false', Reason :: term()}.
args_action(_NameTable, #command_frame{} = CommandFrame, ?WORD_TOKEN(Word)) ->
    process_word(CommandFrame, Word);
args_action(_NameTable, #help_command_frame{} = HelpCommandFrame, ?WORD_TOKEN(Word)) ->
    process_word(HelpCommandFrame, Word);
args_action(_NameTable, #command_frame{} = CommandFrame, ?STRING_TOKEN(String)) ->
    process_string(CommandFrame, String);
args_action(_NameTable, #help_command_frame{} = HelpCommandFrame, ?STRING_TOKEN(String)) ->
    process_string(HelpCommandFrame, String);
args_action(NameTable, #command_frame{} = CommandFrame, ?END_TOKEN) ->
    process_end(NameTable, CommandFrame);
args_action(NameTable, #help_command_frame{} = HelpCommandFrame, ?END_TOKEN) ->
    process_end(NameTable, HelpCommandFrame).

-spec process_word(State :: term(), Word :: string()) ->
    {'true', State :: term()} | {'false', Reason :: term()}.
process_word(#command_frame{items = Items}, Word) ->
    case try_parse_help_command(Word, Items) of
        {true, HelpCommandFrame} -> {true, HelpCommandFrame};
        false ->
            NewCommandFrame = #command_frame{items = [#frame_item{type = word, value = Word}] ++ Items},
            {true, NewCommandFrame}
    end;
process_word(#help_command_frame{arguments = Args} = HelpCommandFrame, Word) ->
    NewArgs = [#argument{type = word, value = Word}] ++ Args,
    {true, HelpCommandFrame#help_command_frame{arguments = NewArgs}}.

-spec process_string(State :: term(), String :: string()) ->
    {'true', State :: term()} | {'false', Reason :: term()}.
process_string(#command_frame{items = Items}, String) ->
    NewCommandFrame = #command_frame{items = [#frame_item{type = string, value = String}] ++ Items},
    {true, NewCommandFrame};
process_string(#help_command_frame{arguments = Args} = HelpCommandFrame, String) ->
    NewArgs = [#argument{type = string, value = String}] ++ Args,
    {true, HelpCommandFrame#help_command_frame{arguments = NewArgs}}.

-spec process_end(NameTable :: term(), State :: term()) ->
    {'true', Result :: term()} | {'false', Reason :: term()}.
process_end(NameTable, #command_frame{items = Items} = Frame) ->
    generate_result(Frame#command_frame{items = lists:reverse(Items)}, NameTable);
process_end(NameTable, #help_command_frame{arguments = Args} = Frame) ->
    generate_result(Frame#help_command_frame{arguments = lists:reverse(Args)}, NameTable).

-spec try_parse_help_command(Word :: string(), Items :: [#frame_item{}]) -> {'true', #help_command_frame{}} | 'false'.
try_parse_help_command(?HELP_COMMAND, Items) ->
    {true, #help_command_frame{items = lists:reverse(Items)}};
try_parse_help_command(Word, Items) ->
    case lists:reverse(Word) of
        [?HELP_COMMAND_CHAR | Prefix] ->
            {true, #help_command_frame{items = lists:reverse(Items), prefix = lists:reverse(Prefix)}};
        _Other -> false
    end.

-spec generate_result(Frame :: #command_frame{} | #help_command_frame{}, NameTable :: name_search_table()) ->
    {'true', Value :: term()} | 'false'.
generate_result(#command_frame{items = Items}, NameTable) ->
    case generate_command_result(Items, NameTable) of
        {true, Module, Args} -> {true, #command{module = Module, arguments = Args}};
        false -> {false, unknown_command}
    end;
generate_result(#help_command_frame{arguments = Args} = HelpCommandFrame, NameTable) ->
    case generate_help_result(HelpCommandFrame, NameTable) of
        false -> {false, unknown_help};
        {true, Module} -> {true, #help_exact_command{module = Module, arguments = Args}};
        Modules -> {true, #help_suitable_command{modules = Modules, arguments = Args}}
    end.

-spec generate_command_result(Items :: [#frame_item{}], NameTable :: name_search_table()) ->
    {'true', Module :: atom(), Args :: [term()]} | 'false'.
generate_command_result(Items, NameTable) ->
    case frame_item_search:search_best(Items, NameTable) of
        {true, Module, RestItems} ->
            Args = lists:map(fun(#frame_item{type = Type, value = Value}) -> #argument{type = Type, value = Value} end, RestItems),
            {true, Module, Args};
        false -> false
    end.

-spec generate_help_result(HelpCommandFrame :: #help_command_frame{}, NameTable :: name_search_table()) ->
    'false' | {'true', Module :: atom()} | [atom()].
generate_help_result(#help_command_frame{items = [], prefix = ""}, NameTable) ->
    {undefined, SearchResult} = frame_item_search:search_suitable([], NameTable),
    lists:map(fun({_SearchItems, Value}) -> Value end, SearchResult);
generate_help_result(#help_command_frame{items = Items, prefix = ""}, NameTable) ->
    frame_item_search:search_exact(Items, NameTable);
generate_help_result(#help_command_frame{items = Items, prefix = Prefix}, NameTable) ->
    {_Module, SearchResult} = frame_item_search:search_suitable(Items, NameTable),
    WordIndex = length(Items) + 1,
    FilterFun = fun(SearchItems, _Value) ->
        {Word, _MinLength} = lists:nth(WordIndex, SearchItems),
        lists:prefix(Prefix, Word)
    end,
    FilterResult = lists:filter(FilterFun, SearchResult),
    lists:map(fun({_SearchItems, Value}) -> Value end, FilterResult).