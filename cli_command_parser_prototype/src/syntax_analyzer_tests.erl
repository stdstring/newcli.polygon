%% @author std-string

-module(syntax_analyzer_tests).

-include("common_defs.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(COMMAND, #nonterminal{name = command}).
-define(ARGS, #nonterminal{name = args}).
-define(WORD_TOKEN, #token{type = word, value = undefined}).
-define(STRING_TOKEN, #token{type = string, value = undefined}).
-define(END_TOKEN, #token{type = 'end', value = ''}).
-define(WORD_TERM, #terminal{type = word, value = undefined}).
-define(STRING_TERM, #terminal{type = string, value = undefined}).
-define(END_TERM, #terminal{type = 'end', value = ''}).

-define(COMMAND_ACTION, fun(ProcessState, Token) -> command_action(ProcessState, Token) end).
-define(ARGS_ACTION, fun(ProcessState, Token) -> args_action(ProcessState, Token) end).

%% ====================================================================
%% Test functions
%% ====================================================================

%%syntax_analyzer_process_test_() ->
%%    SyntaxTable = create_syntax_table(),
%%    [].

simple_syntax_analysis_test() ->
    SyntaxTable = create_syntax_table(),
    io:format(user, "~nparse 'ping 192.168.0.1' :~n", []),
    {ok, _} = syntax_analyzer:process([#token{type = word, value = "ping"}, #token{type = word, value = "192.168.0.1"}, ?END_TOKEN], SyntaxTable, ?COMMAND),
    io:format(user, "~nparse 'exit' :~n", []),
    {ok, _} = syntax_analyzer:process([#token{type = word, value = "exit"}, ?END_TOKEN], SyntaxTable, ?COMMAND),
    io:format(user, "~nparse 'call \"iddqd idkfa\"' :~n", []),
    {ok, _} = syntax_analyzer:process([#token{type = word, value = "call"}, #token{type = string, value = "iddqd idkfa"}, ?END_TOKEN], SyntaxTable, ?COMMAND),
    io:format(user, "~ntry parse '\"iddqd idkfa\"' :~n", []),
    bad_token = syntax_analyzer:process([#token{type = string, value = "iddqd idkfa"}, ?END_TOKEN], SyntaxTable, ?COMMAND),
    io:format(user, "~nparse 'call 666' with unknown token :~n", []),
    bad_token = syntax_analyzer:process([#token{type = word, value = "call"}, #token{type = integer, value = 666}, ?END_TOKEN], SyntaxTable, ?COMMAND).

%% ====================================================================
%% Internal functions
%% ====================================================================

create_syntax_table() ->
    Table =[{{?COMMAND, ?WORD_TOKEN}, {[?WORD_TERM, ?ARGS], ?COMMAND_ACTION}},
            {{?ARGS, ?WORD_TOKEN}, {[?WORD_TERM, ?ARGS], ?ARGS_ACTION}},
            {{?ARGS, ?STRING_TOKEN}, {[?STRING_TERM, ?ARGS], ?ARGS_ACTION}},
            {{?ARGS, ?END_TOKEN}, {[?END_TERM], ?ARGS_ACTION}}],
    dict:from_list(Table).

command_action(#process_state{current_frame = undefined}, #token{type = word, value = Word}) ->
    CommandFrame = #command_frame{items = [#command_frame_item{type = word, value = Word}]},
    #process_state{current_frame = CommandFrame}.

args_action(#process_state{current_frame = #command_frame{items = Items}}, #token{type = word, value = Word}) ->
    NewCommandFrame = #command_frame{items = [#command_frame_item{type = word, value = Word}] ++ Items},
    #process_state{current_frame = NewCommandFrame};
args_action(#process_state{current_frame = #command_frame{items = Items}}, #token{type = string, value = String}) ->
    NewCommandFrame = #command_frame{items = [#command_frame_item{type = string, value = String}] ++ Items},
    #process_state{current_frame = NewCommandFrame};
args_action(#process_state{current_frame = #command_frame{items = Items}}, ?END_TOKEN) ->
    io:format(user, "CommandFrame : ~p~n", [lists:reverse(Items)]),
    #process_state{current_frame = undefined}.