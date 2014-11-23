%% @author std-string

-module(syntax_analyzer_tests).

-include("lexical_defs.hrl").
-include("name_search_defs.hrl").
-include("syntax_defs.hrl").
-include("token_defs.hrl").
-include("syntax_analyzer_defs.hrl").

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

syntax_analyzer_process_test_() ->
    SyntaxTable = syntax_analyzer_config:create(),
    NameTable = name_search_config:create(),
    Config = #syntax_analyzer_config{syntax_table = SyntaxTable, name_table = NameTable},
    [{"parse 'ping 192.168.0.1'",
      ?_assertEqual("ping \"192.168.0.1\"", success_execution([?WORD_TOKEN("ping"), ?WORD_TOKEN("192.168.0.1"), ?END_TOKEN], Config))},
     {"parse 'p 192.168.0.1'",
      ?_assertEqual("ping \"192.168.0.1\"", success_execution([?WORD_TOKEN("p"), ?WORD_TOKEN("192.168.0.1"), ?END_TOKEN], Config))},
     {"parse 'interface \"some interface\"'",
      ?_assertEqual("interface \"some interface\"", success_execution([?WORD_TOKEN("interface"), ?STRING_TOKEN("some interface"), ?END_TOKEN], Config))},
     {"parse 'exit'",
      ?_assertEqual("exit", success_execution([?WORD_TOKEN("exit"), ?END_TOKEN], Config))},
     {"try parse 'call \"iddqd idkfa\"'",
      ?_assertEqual(command_not_found, fail_execution([?WORD_TOKEN("CALL"), ?STRING_TOKEN("iddqd idkfa"), ?END_TOKEN], Config))},
     {"try parse '\"iddqd idkfa\"'",
      ?_assertEqual(bad_token, fail_execution([?STRING_TOKEN("iddqd idkfa"), ?END_TOKEN], Config))},
     {"try parse 'call 666' with unknown token",
      ?_assertEqual(bad_token, fail_execution([?WORD_TOKEN("call"), ?TOKEN(integer, 666), ?END_TOKEN], Config))}].

%% ====================================================================
%% Internal functions
%% ====================================================================

success_execution(TokenList, Config) ->
    {true, Binary} = syntax_analyzer:process(TokenList, ?COMMAND, Config),
    {module, ?EXEC_CONTEXT_MODULE} = code:load_binary(?EXEC_CONTEXT_MODULE, [], Binary),
    ?EXEC_CONTEXT_MODULE:?EXEC_CONTEXT_FUNCTION().

fail_execution(TokenList, Config) ->
    {false, Reason} = syntax_analyzer:process(TokenList, ?COMMAND, Config),
    Reason.