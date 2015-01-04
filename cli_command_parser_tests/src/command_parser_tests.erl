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
    [{"process 'ping XXX'", success_execution("ping XXX", LexConfig, SyntaxConfig, ?PING_MODULE, ["XXX"])},
     {"process 'p XXX'", success_execution("p XXX", LexConfig, SyntaxConfig, ?PING_MODULE, ["XXX"])},
     {"process 'p \"XX\\\"X\" n666'", success_execution("p \"XX\\\"X\" n666", LexConfig, SyntaxConfig, ?PING_MODULE, ["XX\"X", "n666"])},
     {"process 'pong +XXX'", fail_execution("pong +XXX", LexConfig, SyntaxConfig, unsuitable_char)},
     {"process '\"pong\"'", fail_execution("\"pong\"", LexConfig, SyntaxConfig, bad_token)},
     {"process 'pong XXX'", fail_execution("pong XXX", LexConfig, SyntaxConfig, command_not_found)}].

process_help_test_() ->
    LexConfig = lex_analyzer_config:create(true),
    NameData = name_search_config:create(),
    SyntaxConfig = syntax_analyzer_config:create(NameData),
    [{"process '?'", help_execution("?", LexConfig, SyntaxConfig, [], "", [])},
     {"process '? XXX'", help_execution("? XXX", LexConfig, SyntaxConfig, [], "", [?WORD_ARG("XXX")])},
     {"process 'YYY ?'", help_execution("YYY ?", LexConfig, SyntaxConfig, ["YYY"], "", [])},
     {"process 'YYY ? XXX'", help_execution("YYY ? XXX", LexConfig, SyntaxConfig, ["YYY"], "", [?WORD_ARG("XXX")])},
     {"process 'YYY ZZ?'", help_execution("YYY ZZ?", LexConfig, SyntaxConfig, ["YYY"], "ZZ", [])},
     {"process 'YYY ZZ? XXX'", help_execution("YYY ZZ? XXX", LexConfig, SyntaxConfig, ["YYY"], "ZZ", [?WORD_ARG("XXX")])}].

%% ====================================================================
%% Internal functions
%% ====================================================================

success_execution(Source, LexConfig, SyntaxConfig, Module, Args) ->
    Result = command_parser:process(Source, LexConfig, SyntaxConfig),
    ?_assertEqual({true, {Module, Args}}, Result).

fail_execution(Source, LexConfig, SyntaxConfig, Reason) ->
    Result = command_parser:process(Source, LexConfig, SyntaxConfig),
    ?_assertEqual({false, Reason}, Result).

help_execution(Source, LexConfig, SyntaxConfig, Parts, Prefix, Args) ->
    Result = command_parser:process(Source, LexConfig, SyntaxConfig),
    ?_assertEqual({true, #help_command{parts = Parts, prefix = Prefix, arguments = Args}}, Result).
