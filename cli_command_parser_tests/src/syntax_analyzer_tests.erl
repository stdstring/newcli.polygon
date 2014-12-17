%% @author std-string

-module(syntax_analyzer_tests).

-include("frame_defs.hrl").
-include("lexical_defs.hrl").
-include("name_search_defs.hrl").
-include("syntax_defs.hrl").
-include("token_defs.hrl").
-include("module_defs.hrl").
-include("syntax_analyzer_defs.hrl").

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

syntax_analyzer_process_test_() ->
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

%% ====================================================================
%% Internal functions
%% ====================================================================

success_execution(TokenList, Config, Module, Args) ->
    Result = syntax_analyzer:process(TokenList, Config),
    ?_assertEqual({true, {Module, Args}}, Result).

fail_execution(TokenList, Config, Reason) ->
    Result = syntax_analyzer:process(TokenList, Config),
    ?_assertEqual({false, Reason}, Result).