%% @author std-string

-module(command_parser_tests).

-include("frame_defs.hrl").
-include("lexical_defs.hrl").
-include("syntax_defs.hrl").

-include("command_defs.hrl").
-include("syntax_analyzer_defs.hrl").
-include("token_defs.hrl").

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

process_test_() ->
    LexConfig = lex_analyzer_config:create(true),
    NameData = name_search_config:create(),
    SyntaxConfig = syntax_analyzer_config:create(NameData),
    [{"process 'ping XXX'",
      success_execution("ping XXX", LexConfig, SyntaxConfig, ?PING_COMMAND, [?WORD_ARG("XXX")])},
     {"process 'p XXX'",
      success_execution("p XXX", LexConfig, SyntaxConfig, ?PING_COMMAND, [?WORD_ARG("XXX")])},
     {"process 'p \"XX\\\"X\" n666'",
      success_execution("p \"XX\\\"X\" n666", LexConfig, SyntaxConfig, ?PING_COMMAND, [?STRING_ARG("XX\"X"), ?WORD_ARG("n666")])},
     {"process 'pong +XXX'",
      fail_execution("pong +XXX", LexConfig, SyntaxConfig, unsuitable_char)},
     {"process '\"pong\"'",
      fail_execution("\"pong\"", LexConfig, SyntaxConfig, bad_token)},
     {"process 'pong XXX'",
      fail_execution("pong XXX", LexConfig, SyntaxConfig, unknown_command)}].

%% ====================================================================
%% Internal functions
%% ====================================================================

success_execution(Source, LexConfig, SyntaxConfig, Module, Args) ->
    Result = command_parser:process(Source, LexConfig, SyntaxConfig),
    ?_assertEqual({true, #command{module = Module, arguments = Args}}, Result).

fail_execution(Source, LexConfig, SyntaxConfig, Reason) ->
    Result = command_parser:process(Source, LexConfig, SyntaxConfig),
    ?_assertEqual({false, Reason}, Result).