%% @author std-string

-module(command_parser_tests).

-include("frame_defs.hrl").
-include("lexical_defs.hrl").
-include("name_search_defs.hrl").
-include("syntax_defs.hrl").

-include("function_defs.hrl").
-include("syntax_analyzer_defs.hrl").
-include("token_defs.hrl").

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

command_parser_process_test_() ->
    LexData = lex_analyzer_config:create(),
    NameData = name_search_config:create(),
    SyntaxData = syntax_analyzer_config:create(),
    LexConfig = #lex_analyzer_config{token_parsers_config = LexData, skip_whitespaces = true},
    SyntaxConfig = #syntax_analyzer_config{syntax_table = SyntaxData, start_symbol = ?COMMAND, name_table = NameData},
    [].

%% ====================================================================
%% Internal functions
%% ====================================================================

success_execution(Source, LexConfig, SyntaxConfig, Function, Args) ->
    Result = command_parser:process(Source, LexConfig, SyntaxConfig),
    ?_assertEqual({true, {?MODULE_NAME, Function, Args}}, Result).

fail_execution(Source, LexConfig, SyntaxConfig, Reason) ->
    Result = command_parser:process(Source, LexConfig, SyntaxConfig),
    ?_assertEqual({false, Reason}, Result).