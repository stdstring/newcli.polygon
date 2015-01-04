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
args_action(_NameTable, #help_command{} = HelpCommand, ?WORD_TOKEN(Word)) ->
    process_word(HelpCommand, Word);
args_action(_NameTable, #command_frame{} = CommandFrame, ?STRING_TOKEN(String)) ->
    process_string(CommandFrame, String);
args_action(_NameTable, #help_command{} = HelpCommand, ?STRING_TOKEN(String)) ->
    process_string(HelpCommand, String);
args_action(NameTable, #command_frame{} = CommandFrame, ?END_TOKEN) ->
    process_end(NameTable, CommandFrame);
args_action(NameTable, #help_command{} = HelpCommand, ?END_TOKEN) ->
    process_end(NameTable, HelpCommand).

-spec generate_result(Items :: [#frame_item{}], NameTable :: name_search_table()) ->
    {'true', Module :: atom(), Args :: [term()]} | 'false'.
generate_result(Items, NameTable) ->
    case frame_item_search:search_best(Items, NameTable) of
        {true, Module, RestItems} ->
            Args = lists:map(fun(#frame_item{type = Type, value = Value}) -> #argument{type = Type, value = Value} end, RestItems),
            {true, Module, Args};
        false -> false
    end.

-spec process_word(State :: term(), Word :: string()) ->
    {'true', State :: term()} | {'false', Reason :: term()}.
process_word(#command_frame{items = Items}, Word) ->
    case try_parse_help_command(Word, Items) of
        {true, HelpCommand} -> {true, HelpCommand};
        false ->
            NewCommandFrame = #command_frame{items = [#frame_item{type = word, value = Word}] ++ Items},
            {true, NewCommandFrame}
    end;
process_word(#help_command{arguments = Args} = HelpCommand, Word) ->
    NewArgs = [#argument{type = word, value = Word}] ++ Args,
    {true, HelpCommand#help_command{arguments = NewArgs}}.

-spec process_string(State :: term(), String :: string()) ->
    {'true', State :: term()} | {'false', Reason :: term()}.
process_string(#command_frame{items = Items}, String) ->
    NewCommandFrame = #command_frame{items = [#frame_item{type = string, value = String}] ++ Items},
    {true, NewCommandFrame};
process_string(#help_command{arguments = Args} = HelpCommand, String) ->
    NewArgs = [#argument{type = string, value = String}] ++ Args,
    {true, HelpCommand#help_command{arguments = NewArgs}}.

-spec process_end(NameTable :: term(), State :: term()) ->
    {'true', Result :: term()} | {'false', Reason :: term()}.
process_end(NameTable, #command_frame{items = Items}) ->
    case generate_result(lists:reverse(Items), NameTable) of
        {true, Module, Args} -> {true, #command{module = Module, arguments = Args}};
        false -> {false, command_not_found}
    end;
process_end(_NameTable, #help_command{arguments = Args} = HelpCommand) ->
    FinalHelpCommand = HelpCommand#help_command{arguments = lists:reverse(Args)},
    {true, FinalHelpCommand}.

-spec try_parse_help_command(Word :: string(), Items :: [#frame_item{}]) -> {'true', #help_command{}} | 'false'.
try_parse_help_command(?HELP_COMMAND, Items) ->
    Parts = lists:map(fun(#frame_item{value = Value}) -> Value end, lists:reverse(Items)),
    {true, #help_command{parts = Parts}};
try_parse_help_command(Word, Items) ->
    case lists:reverse(Word) of
        [?HELP_COMMAND_CHAR | Prefix] ->
            Parts = lists:map(fun(#frame_item{value = Value}) -> Value end, lists:reverse(Items)),
            {true, #help_command{parts = Parts, prefix = lists:reverse(Prefix)}};
        _Other -> false
    end.