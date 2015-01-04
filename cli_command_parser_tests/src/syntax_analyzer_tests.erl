%% @author std-string

-module(syntax_analyzer_tests).

-include("frame_defs.hrl").
-include("lexical_defs.hrl").
-include("syntax_defs.hrl").
-include("token_defs.hrl").
-include("module_defs.hrl").
-include("syntax_analyzer_defs.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(WORD_ARG(Value), #argument{type = word, value = Value}).

%% ====================================================================
%% Test functions
%% ====================================================================

process_test_() ->
    NameTable = name_search_config:create(),
    Config = syntax_analyzer_config:create(NameTable),
    [{"parse 'ping 192.168.0.1'",
      success_execution([?WORD_TOKEN("ping"), ?WORD_TOKEN("192.168.0.1"), ?END_TOKEN], Config, ?PING_MODULE, ["192.168.0.1"])},
     {"parse 'p 192.168.0.1'",
      success_execution([?WORD_TOKEN("p"), ?WORD_TOKEN("192.168.0.1"), ?END_TOKEN], Config, ?PING_MODULE, ["192.168.0.1"])},
     {"parse 'interface \"some interface\"'",
      success_execution([?WORD_TOKEN("interface"), ?STRING_TOKEN("some interface"), ?END_TOKEN], Config, ?INTERFACE_MODULE, ["some interface"])},
     {"parse 'exit'",
      success_execution([?WORD_TOKEN("exit"), ?END_TOKEN], Config, ?EXIT_MODULE, [])},
     {"try parse 'call \"iddqd idkfa\"'",
      fail_execution([?WORD_TOKEN("CALL"), ?STRING_TOKEN("iddqd idkfa"), ?END_TOKEN], Config, command_not_found)},
     {"try parse '\"iddqd idkfa\"'",
      fail_execution([?STRING_TOKEN("iddqd idkfa"), ?END_TOKEN], Config, bad_token)},
     {"try parse 'call 666' with unknown token",
      fail_execution([?WORD_TOKEN("call"), ?TOKEN(integer, 666), ?END_TOKEN], Config, bad_token)}].

process_help_test_() ->
    NameTable = name_search_config:create(),
    Config = syntax_analyzer_config:create(NameTable),
    [{"parse '?'",
      help_execution([?WORD_TOKEN("?"), ?END_TOKEN], Config, [], "", [])},
     {"parse '? XXX'",
      help_execution([?WORD_TOKEN("?"), ?WORD_TOKEN("XXX"), ?END_TOKEN], Config, [], "", [?WORD_ARG("XXX")])},
     {"parse 'YYY ?'",
      help_execution([?WORD_TOKEN("YYY"), ?WORD_TOKEN("?"), ?END_TOKEN], Config, ["YYY"], "", [])},
     {"parse 'YYY ? XXX'",
      help_execution([?WORD_TOKEN("YYY"), ?WORD_TOKEN("?"), ?WORD_TOKEN("XXX"), ?END_TOKEN], Config, ["YYY"], "", [?WORD_ARG("XXX")])},
      {"parse 'YYY ZZ?'",
      help_execution([?WORD_TOKEN("YYY"), ?WORD_TOKEN("ZZ?"), ?END_TOKEN], Config, ["YYY"], "ZZ", [])},
     {"parse 'YYY ZZ? XXX'",
      help_execution([?WORD_TOKEN("YYY"), ?WORD_TOKEN("ZZ?"), ?WORD_TOKEN("XXX"), ?END_TOKEN], Config, ["YYY"], "ZZ", [?WORD_ARG("XXX")])}].

%% ====================================================================
%% Internal functions
%% ====================================================================

success_execution(TokenList, Config, Module, Args) ->
    Result = syntax_analyzer:process(TokenList, Config),
    ?_assertEqual({true, {Module, Args}}, Result).

fail_execution(TokenList, Config, Reason) ->
    Result = syntax_analyzer:process(TokenList, Config),
    ?_assertEqual({false, Reason}, Result).

help_execution(TokenList, Config, Parts, Prefix, Args) ->
    Result = syntax_analyzer:process(TokenList, Config),
    ?_assertEqual({true, #help_command{parts = Parts, prefix = Prefix, arguments = Args}}, Result).