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
    LexConfig = lex_analyzer_config:create(true),
    NameData = name_search_config:create(),
    SyntaxConfig = syntax_analyzer_config:create(NameData),
    [{"process 'ping XXX'", success_execution("ping XXX", LexConfig, SyntaxConfig, ?PING_FUNCTION, ["XXX"])},
     {"process 'p XXX'", success_execution("p XXX", LexConfig, SyntaxConfig, ?PING_FUNCTION, ["XXX"])},
     {"process 'p \"XX\\\"X\" n666'", success_execution("p \"XX\\\"X\" n666", LexConfig, SyntaxConfig, ?PING_FUNCTION, ["XX\"X", "n666"])},
     {"process 'pong -XXX'", fail_execution("pong -XXX", LexConfig, SyntaxConfig, unsuitable_char)},
     {"process '\"pong\"'", fail_execution("\"pong\"", LexConfig, SyntaxConfig, bad_token)},
     {"process 'pong XXX'", fail_execution("pong XXX", LexConfig, SyntaxConfig, command_not_found)}].

%% ====================================================================
%% Internal functions
%% ====================================================================

success_execution(Source, LexConfig, SyntaxConfig, Function, Args) ->
    Result = command_parser:process(Source, LexConfig, SyntaxConfig),
    ?_assertEqual({true, {?MODULE_NAME, Function, Args}}, Result).

fail_execution(Source, LexConfig, SyntaxConfig, Reason) ->
    Result = command_parser:process(Source, LexConfig, SyntaxConfig),
    ?_assertEqual({false, Reason}, Result).