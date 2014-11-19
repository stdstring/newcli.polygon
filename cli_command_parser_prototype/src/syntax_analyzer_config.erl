-module(syntax_analyzer_config).

-export([create_config/0]).

-include("common_defs.hrl").
-include("token_defs.hrl").
-include("syntax_analyzer_defs.hrl").

-define(WORD_TEMPLATE, ?WORD_TOKEN(undefined)).
-define(STRING_TEMPLATE, ?STRING_TOKEN(undefined)).

-define(COMMAND_ACTION, fun(NameTable, ProcessState, Token) -> command_action(NameTable, ProcessState, Token) end).
-define(ARGS_ACTION, fun(NameTable, ProcessState, Token) -> args_action(NameTable, ProcessState, Token) end).

%% ====================================================================
%% API functions
%% ====================================================================

create_config() ->
    Table =[{{?COMMAND, ?WORD_TEMPLATE}, {[?WORD_TERM, ?ARGS], ?COMMAND_ACTION}},
            {{?ARGS, ?WORD_TEMPLATE}, {[?WORD_TERM, ?ARGS], ?ARGS_ACTION}},
            {{?ARGS, ?STRING_TEMPLATE}, {[?STRING_TERM, ?ARGS], ?ARGS_ACTION}},
            {{?ARGS, ?END_TOKEN}, {[?END_TERM], ?ARGS_ACTION}}],
    dict:from_list(Table).

%% ====================================================================
%% Internal functions
%% ====================================================================

command_action(_NameTable, #process_state{current_frame = undefined}, #token{type = word, value = Word}) ->
    CommandFrame = #command_frame{items = [#frame_item{type = word, value = Word}]},
    {true, #process_state{current_frame = CommandFrame}}.

args_action(_NameTable, #process_state{current_frame = #command_frame{items = Items}}, #token{type = word, value = Word}) ->
    NewCommandFrame = #command_frame{items = [#frame_item{type = word, value = Word}] ++ Items},
    {true, #process_state{current_frame = NewCommandFrame}};
args_action(_NameTable, #process_state{current_frame = #command_frame{items = Items}}, #token{type = string, value = String}) ->
    NewCommandFrame = #command_frame{items = [#frame_item{type = string, value = String}] ++ Items},
    {true, #process_state{current_frame = NewCommandFrame}};
args_action(NameTable, #process_state{current_frame = #command_frame{items = Items}}, ?END_TOKEN) ->
    case generate_code(lists:reverse(Items), NameTable) of
        {true, Binary} -> {true, #process_state{current_frame = undefined, binary_code = Binary}};
        false -> {false, command_not_found}
    end.

generate_code(Items, NameTable) ->
    case frame_item_search:search_best(Items, NameTable) of
        {true, CommandModule, CommandFunction, CommandArgs} ->
            {ok, ?EXEC_CONTEXT_MODULE, Binary} = code_generator:generate(?EXEC_CONTEXT_MODULE, ?EXEC_CONTEXT_FUNCTION, {CommandModule, CommandFunction, CommandArgs}),
            {true, Binary};
        false -> false
    end.