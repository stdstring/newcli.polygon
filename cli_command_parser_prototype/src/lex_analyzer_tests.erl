%% @author std-string

-module(lex_analyzer_tests).

-include_lib("eunit/include/eunit.hrl").

-include("common_defs.hrl").
-include("token_defs.hrl").

-define(APPENDER, fun(Char, Buffer) -> [Char] ++ Buffer end).
-define(EMPTY_APPENDER, fun(_Char, Buffer) -> Buffer end).

-define(WORD_PREDICATE, fun(Char) -> word_body_predicate(Char) end).
-define(STR_BODY_PREDICATE, fun(Char) -> Char /= $" end).
-define(STR_QUOTE_PREDICATE, fun(Char) -> Char == $" end).
-define(SPACE_PREDICATE, fun(Char) -> lists:member(Char, " \t") end).

-define(WORD_INIT_STATE, word_init_state).
-define(WORD_BODY_STATE, word_body_state).
-define(STR_INIT_STATE, str_init_state).
-define(STR_BODY_STATE, str_body_state).
-define(STR_FINAL_STATE, str_final_state).
-define(SPACE_INIT_STATE, space_init_state).
-define(SPACE_BODY_STATE, space_body_state).
-define(IF_INIT_STATE, if_init_state).
-define(IF_INNER_STATE, if_inner_state).
-define(IF_FINAL_STATE, if_final_state).

%% ====================================================================
%% Test functions
%% ====================================================================

parse_test_() ->
    ConfigList = create_config(),
    [check_success("ping XXX", [?WORD("ping"), ?WHITESPACE, ?WORD("XXX"), ?END_TOKEN], ConfigList, false),
     check_success("ping XXX", [?WORD("ping"), ?WORD("XXX"), ?END_TOKEN], ConfigList, true),
     check_success("ping \"impulse 9\"", [?WORD("ping"), ?WHITESPACE, ?STRING("impulse 9"), ?END_TOKEN], ConfigList, false),
     check_success("ping \"impulse 9\"", [?WORD("ping"), ?STRING("impulse 9"), ?END_TOKEN], ConfigList, true),
     check_success("ping \t\t XXX", [?WORD("ping"), ?WHITESPACE, ?WORD("XXX"), ?END_TOKEN], ConfigList, false),
     check_success("ping \t\t XXX", [?WORD("ping"), ?WORD("XXX"), ?END_TOKEN], ConfigList, true)].

different_token_parsers_test_() ->
    [check_success("parse \"if\". word parser first", "if", [?TOKEN(keyword, "if"), ?END_TOKEN], [create_word_config(), create_keyword_config()], false),
     check_success("parse \"if\". keyword parser first", "if", [?WORD("if"), ?END_TOKEN], [create_keyword_config(), create_word_config()], false)].

%% ====================================================================
%% Internal functions
%% ====================================================================

check_success(Source, Expected, ConfigList, true) ->
    Description = lists:flatten(io_lib:format("parse ~p with skip whitespace tokens", [Source])),
    check_success(Description, Source, Expected, ConfigList, true);
check_success(Source, Expected, ConfigList, false) ->
    Description = lists:flatten(io_lib:format("parse ~p  without skip whitespace tokens", [Source])),
    check_success(Description, Source, Expected, ConfigList, false).

check_success(Description, Source, Expected, ConfigList, SkipWhitespaces) ->
    {Description, ?_assertEqual({true, Expected}, lex_analyzer:parse(Source, ConfigList, SkipWhitespaces))}.

create_word_config() ->
    TransitionTable = [#transition{from_state = ?WORD_INIT_STATE,
                                   char_predicate = ?WORD_PREDICATE,
                                   to_state = ?WORD_BODY_STATE,
                                   char_appender = ?APPENDER},
                       #transition{from_state = ?WORD_BODY_STATE,
                                   char_predicate = ?WORD_PREDICATE,
                                   to_state = ?WORD_BODY_STATE,
                                   char_appender = ?APPENDER}],
    FinalStates = [?WORD_BODY_STATE],
    TokenFactory = fun(#token_parser_state{current_state = ?WORD_BODY_STATE, recognized_buffer = Buffer}) ->
        #token{type = word, value = lists:reverse(Buffer)}
    end,
    #token_parser_config{init_state = ?WORD_INIT_STATE,
                         transitions = TransitionTable,
                         final_states = FinalStates,
                         token_factory = TokenFactory}.

create_string_config() ->
    TransitionTable = [#transition{from_state = ?STR_INIT_STATE,
                                   char_predicate = ?STR_QUOTE_PREDICATE,
                                   to_state = ?STR_BODY_STATE,
                                   char_appender = ?EMPTY_APPENDER},
                       #transition{from_state = ?STR_BODY_STATE,
                                   char_predicate = ?STR_BODY_PREDICATE,
                                   to_state = ?STR_BODY_STATE,
                                   char_appender = ?APPENDER},
                       #transition{from_state = ?STR_BODY_STATE,
                                   char_predicate = ?STR_QUOTE_PREDICATE,
                                   to_state = ?STR_FINAL_STATE,
                                   char_appender = ?EMPTY_APPENDER}],
    FinalStates = [?STR_FINAL_STATE],
    TokenFactory = fun(#token_parser_state{current_state = ?STR_FINAL_STATE, recognized_buffer = Buffer}) ->
        #token{type = string, value = lists:reverse(Buffer)}
    end,
    #token_parser_config{init_state = ?STR_INIT_STATE,
                         transitions = TransitionTable,
                         final_states = FinalStates,
                         token_factory = TokenFactory}.

create_space_config() ->
    TransitionTable = [#transition{from_state = ?SPACE_INIT_STATE,
                                   char_predicate = ?SPACE_PREDICATE,
                                   to_state = ?SPACE_BODY_STATE,
                                   char_appender = ?EMPTY_APPENDER},
                       #transition{from_state = ?SPACE_BODY_STATE,
                                   char_predicate = ?SPACE_PREDICATE,
                                   to_state = ?SPACE_BODY_STATE,
                                   char_appender = ?EMPTY_APPENDER}],
    FinalStates = [?SPACE_BODY_STATE],
    TokenFactory = fun(#token_parser_state{current_state = ?SPACE_BODY_STATE}) ->
        #token{type = whitespace, value = ""}
    end,
    #token_parser_config{init_state = ?SPACE_INIT_STATE,
                         transitions = TransitionTable,
                         final_states = FinalStates,
                         token_factory = TokenFactory}.

create_keyword_config() ->
    TransitionTable = [#transition{from_state = ?IF_INIT_STATE,
                                   char_predicate = fun(Char) -> Char == $i end,
                                   to_state = ?IF_INNER_STATE,
                                   char_appender = ?APPENDER},
                       #transition{from_state = ?IF_INNER_STATE,
                                   char_predicate = fun(Char) -> Char == $f end,
                                   to_state = ?IF_FINAL_STATE,
                                   char_appender = ?APPENDER}],
    FinalStates = [?IF_FINAL_STATE],
    TokenFactory = fun(#token_parser_state{current_state = ?IF_FINAL_STATE}) ->
        #token{type = keyword, value = "if"}
    end,
    #token_parser_config{init_state = ?IF_INIT_STATE,
                         transitions = TransitionTable,
                         final_states = FinalStates,
                         token_factory = TokenFactory}.


create_config() ->
    [create_word_config(), create_string_config(), create_space_config()].

word_body_predicate(Char) ->
    char_category:is_letter(Char) orelse
    char_category:is_digit(Char) orelse
    lists:member(Char, "\\.,-").