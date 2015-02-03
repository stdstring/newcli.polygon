%% @author std-string

-module(command_parser_tests).

-include("frame_defs.hrl").
-include("lexical_defs.hrl").
-include("syntax_defs.hrl").

-include("module_defs.hrl").
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
      success_execution("ping XXX", LexConfig, SyntaxConfig, ?PING_MODULE, [?WORD_ARG("XXX")])},
     {"process 'p XXX'",
      success_execution("p XXX", LexConfig, SyntaxConfig, ?PING_MODULE, [?WORD_ARG("XXX")])},
     {"process 'p \"XX\\\"X\" n666'",
      success_execution("p \"XX\\\"X\" n666", LexConfig, SyntaxConfig, ?PING_MODULE, [?STRING_ARG("XX\"X"), ?WORD_ARG("n666")])},
     {"process 'pong +XXX'",
      fail_execution("pong +XXX", LexConfig, SyntaxConfig, unsuitable_char)},
     {"process '\"pong\"'",
      fail_execution("\"pong\"", LexConfig, SyntaxConfig, bad_token)},
     {"process 'pong XXX'",
      fail_execution("pong XXX", LexConfig, SyntaxConfig, unknown_command)}].

process_help_test_() ->
    LexConfig = lex_analyzer_config:create(true),
    NameData = name_search_config:create(),
    SyntaxConfig = syntax_analyzer_config:create(NameData),
    [{"process '?'",
      help_suitable_execution("?", LexConfig, SyntaxConfig, ?ALL_MODULES, [])},
     {"process '? XXX'",
      help_suitable_execution("? XXX", LexConfig, SyntaxConfig, ?ALL_MODULES, [?WORD_ARG("XXX")])},
     {"process 'YYY ?'",
      fail_help_exact_execution("YYY ?", LexConfig, SyntaxConfig)},
     {"process 'YYY ? XXX'",
      fail_help_exact_execution("YYY ? XXX", LexConfig, SyntaxConfig)},
     {"process 'ping ?'",
      success_help_exact_execution("ping ?", LexConfig, SyntaxConfig, ?PING_MODULE, [])},
     {"process 'ping ? XXX'",
      success_help_exact_execution("ping ? XXX", LexConfig, SyntaxConfig, ?PING_MODULE, [?WORD_ARG("XXX")])},
     {"process 'YYY ZZ?'",
      help_suitable_execution("YYY ZZ?", LexConfig, SyntaxConfig, [], [])},
     {"process 'YYY ZZ? XXX'",
      help_suitable_execution("YYY ZZ? XXX", LexConfig, SyntaxConfig, [], [?WORD_ARG("XXX")])},
     {"process 'i?'",
      help_suitable_execution("i?", LexConfig, SyntaxConfig, [?INTERFACE_MODULE, ?IFRANGE_MODULE], [])},
     {"process 'i? XXX'",
      help_suitable_execution("i? XXX", LexConfig, SyntaxConfig, [?INTERFACE_MODULE, ?IFRANGE_MODULE], [?WORD_ARG("XXX")])}].

%% ====================================================================
%% Internal functions
%% ====================================================================

success_execution(Source, LexConfig, SyntaxConfig, Module, Args) ->
    Result = command_parser:process(Source, LexConfig, SyntaxConfig),
    ?_assertEqual({true, #command{module = Module, arguments = Args}}, Result).

fail_execution(Source, LexConfig, SyntaxConfig, Reason) ->
    Result = command_parser:process(Source, LexConfig, SyntaxConfig),
    ?_assertEqual({false, Reason}, Result).

help_suitable_execution(Source, LexConfig, SyntaxConfig, Modules, Args) ->
    Result = command_parser:process(Source, LexConfig, SyntaxConfig),
    ?_assertEqual({true, #help_suitable_command{modules = Modules, arguments = Args}}, Result).

success_help_exact_execution(Source, LexConfig, SyntaxConfig, Module, Args) ->
    Result = command_parser:process(Source, LexConfig, SyntaxConfig),
    ?_assertEqual({true, #help_exact_command{module = Module, arguments = Args}}, Result).

fail_help_exact_execution(Source, LexConfig, SyntaxConfig) ->
    Result = command_parser:process(Source, LexConfig, SyntaxConfig),
    ?_assertEqual({false, unknown_help}, Result).