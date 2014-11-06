%% @author std-string

-module(syntax_analyzer_tests).

-include("common_defs.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(COMMAND, #nonterminal{name = "Command"}).
-define(ARGS, #nonterminal{name = "Args"}).
-define(WORD_TOKEN, #token{type = "Word", value = undefined}).
-define(STRING_TOKEN, #token{type = "String", value = undefined}).
-define(END_TOKEN, #token{type = "End", value = ''}).
-define(WORD_TERM, #terminal{type = "Word", value = undefined}).
-define(STRING_TERM, #terminal{type = "String", value = undefined}).
-define(END_TERM, #terminal{type = "End", value = ''}).

%% ====================================================================
%% Test functions
%% ====================================================================

%%syntax_analyzer_process_test_() ->
%%    SyntaxTable = create_syntax_table(),
%%    [].

syntax_analyzer_process_test() ->
    SyntaxTable = create_syntax_table(),
    ok = syntax_analyzer:process([#token{type = "Word", value = "ping"}, #token{type = "Word", value = "192.168.0.1"}, ?END_TOKEN], SyntaxTable, ?COMMAND),
    ok = syntax_analyzer:process([#token{type = "Word", value = "exit"}, ?END_TOKEN], SyntaxTable, ?COMMAND).

%% ====================================================================
%% Internal functions
%% ====================================================================

create_syntax_table() ->
    Table =[{{?COMMAND, ?WORD_TOKEN}, [?WORD_TERM, ?ARGS]},
            {{?ARGS, ?WORD_TOKEN}, [?WORD_TERM, ?ARGS]},
            {{?ARGS, ?STRING_TOKEN}, [?STRING_TERM, ?ARGS]},
            {{?ARGS, ?END_TOKEN}, []}],
    dict:from_list(Table).