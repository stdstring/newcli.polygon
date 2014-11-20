%% @author std-string

-module(lex_analyzer_tests).

-include_lib("eunit/include/eunit.hrl").

-include("common_defs.hrl").
-include("token_defs.hrl").

-define(IF_INIT_STATE, if_init_state).
-define(IF_INNER_STATE, if_inner_state).
-define(IF_FINAL_STATE, if_final_state).

-define(PARSE_WITHOUT_WHITESPACE(Cmd), "parse '" ++ Cmd ++ "' with skip whitespace tokens").
-define(PARSE_WITH_WHITESPACE(Cmd), "parse '" ++ Cmd ++ "' without skip whitespace tokens").

%% ====================================================================
%% Test functions
%% ====================================================================

parse_test_() ->
    ConfigList = lex_analyzer_config:create_config(),
    [check_success("ping XXX", [?WORD_TOKEN("ping"), ?WHITESPACE_TOKEN, ?WORD_TOKEN("XXX"), ?END_TOKEN], ConfigList, false),
     check_success("ping XXX", [?WORD_TOKEN("ping"), ?WORD_TOKEN("XXX"), ?END_TOKEN], ConfigList, true),
     check_success("ping \"impulse 9\"", [?WORD_TOKEN("ping"), ?WHITESPACE_TOKEN, ?STRING_TOKEN("impulse 9"), ?END_TOKEN], ConfigList, false),
     check_success("ping \"impulse 9\"", [?WORD_TOKEN("ping"), ?STRING_TOKEN("impulse 9"), ?END_TOKEN], ConfigList, true),
     check_success(?PARSE_WITH_WHITESPACE("ping \\t\\t XXX"), "ping \t\t XXX", [?WORD_TOKEN("ping"), ?WHITESPACE_TOKEN, ?WORD_TOKEN("XXX"), ?END_TOKEN], ConfigList, false),
     check_success(?PARSE_WITHOUT_WHITESPACE("ping \\t\\t XXX"), "ping \t\t XXX", [?WORD_TOKEN("ping"), ?WORD_TOKEN("XXX"), ?END_TOKEN], ConfigList, true),
     check_success("c t", [?WORD_TOKEN("c"), ?WHITESPACE_TOKEN, ?WORD_TOKEN("t"), ?END_TOKEN], ConfigList, false),
     check_success("c t", [?WORD_TOKEN("c"), ?WORD_TOKEN("t"), ?END_TOKEN], ConfigList, true),
     check_fail("ping +", unsuitable_char, ConfigList),
     check_fail("ping +XXX", unsuitable_char, ConfigList),
     check_fail("ping \"", bad_input, ConfigList),
     check_fail("ping \"XXX", bad_input, ConfigList)].

different_token_parsers_test_() ->
    ConfigList = lex_analyzer_config:create_config(),
    [check_success("parse 'if'. word parser first", "if", [?TOKEN(keyword, "if"), ?END_TOKEN], ConfigList ++ [create_keyword_config()], false),
     check_success("parse 'if'. keyword parser first", "if", [?WORD_TOKEN("if"), ?END_TOKEN], [create_keyword_config()] ++ ConfigList, false)].

%% ====================================================================
%% Internal functions
%% ====================================================================

check_success(Source, Expected, ConfigList, true) ->
    Description = ?PARSE_WITHOUT_WHITESPACE(Source),
    check_success(Description, Source, Expected, ConfigList, true);
check_success(Source, Expected, ConfigList, false) ->
    Description = ?PARSE_WITH_WHITESPACE(Source),
    check_success(Description, Source, Expected, ConfigList, false).

check_success(Description, Source, Expected, ConfigList, SkipWhitespaces) ->
    {Description, ?_assertEqual({true, Expected}, lex_analyzer:parse(Source, ConfigList, SkipWhitespaces))}.

check_fail(Source, Reason, ConfigList) ->
    Description = lists:flatten(io_lib:format("try parse '~s'", [Source])),
    check_fail(Description, Source, Reason, ConfigList).

check_fail(Description, Source, Reason, ConfigList) ->
    {Description, ?_assertEqual({false, Reason}, lex_analyzer:parse(Source, ConfigList, false))}.

create_keyword_config() ->
    TransitionTable = [#transition{from_state = ?IF_INIT_STATE,
                                   char_predicate = fun(Char) -> Char == $i end,
                                   to_state = ?IF_INNER_STATE,
                                   char_appender = fun(Char, Buffer) -> [Char] ++ Buffer end},
                       #transition{from_state = ?IF_INNER_STATE,
                                   char_predicate = fun(Char) -> Char == $f end,
                                   to_state = ?IF_FINAL_STATE,
                                   char_appender = fun(Char, Buffer) -> [Char] ++ Buffer end}],
    FinalStates = [?IF_FINAL_STATE],
    TokenFactory = fun(#token_parser_state{current_state = ?IF_FINAL_STATE}) ->
        #token{type = keyword, value = "if"}
    end,
    #token_parser_config{init_state = ?IF_INIT_STATE,
                         transitions = TransitionTable,
                         final_states = FinalStates,
                         token_factory = TokenFactory}.