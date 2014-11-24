-module(syntax_analyzer_config).

-export([create/0]).

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

create() ->
    Table =[{{?COMMAND, ?WORD_TEMPLATE}, {[?WORD_TERM, ?ARGS], ?COMMAND_ACTION}},
            {{?ARGS, ?WORD_TEMPLATE}, {[?WORD_TERM, ?ARGS], ?ARGS_ACTION}},
            {{?ARGS, ?STRING_TEMPLATE}, {[?STRING_TERM, ?ARGS], ?ARGS_ACTION}},
            {{?ARGS, ?END_TOKEN}, {[?END_TERM], ?ARGS_ACTION}}],
    dict:from_list(Table).

%% ====================================================================
%% Internal functions
%% ====================================================================

command_action(_NameTable, #syntax_process_state{current_frame = undefined}, ?WORD_TOKEN(Word)) ->
    CommandFrame = #command_frame{items = [#frame_item{type = word, value = Word}]},
    {true, #syntax_process_state{current_frame = CommandFrame}}.

args_action(_NameTable, #syntax_process_state{current_frame = #command_frame{items = Items}}, ?WORD_TOKEN(Word)) ->
    NewCommandFrame = #command_frame{items = [#frame_item{type = word, value = Word}] ++ Items},
    {true, #syntax_process_state{current_frame = NewCommandFrame}};
args_action(_NameTable, #syntax_process_state{current_frame = #command_frame{items = Items}}, ?STRING_TOKEN(String)) ->
    NewCommandFrame = #command_frame{items = [#frame_item{type = string, value = String}] ++ Items},
    {true, #syntax_process_state{current_frame = NewCommandFrame}};
args_action(NameTable, #syntax_process_state{current_frame = #command_frame{items = Items}}, ?END_TOKEN) ->
    case generate_code(lists:reverse(Items), NameTable) of
        {true, Binary} -> {true, #syntax_process_state{current_frame = undefined, result = Binary}};
        false -> {false, command_not_found}
    end.

generate_code(Items, NameTable) ->
    case frame_item_search:search_best(Items, NameTable) of
        {true, CommandModule, CommandFunction, CommandArgs} ->
            {ok, ?EXEC_CONTEXT_MODULE, Binary} = code_generator:generate(?EXEC_CONTEXT_MODULE, ?EXEC_CONTEXT_FUNCTION, {CommandModule, CommandFunction, CommandArgs}),
            {true, Binary};
        false -> false
    end.