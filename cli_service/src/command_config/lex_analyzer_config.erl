%% @author std-string

-module(lex_analyzer_config).

-export([create/1]).

-include("lexical_defs.hrl").
-include("token_defs.hrl").

%% word token
-define(WORD_INIT_STATE, word_init_state).
-define(WORD_BODY_STATE, word_body_state).
%% string token
-define(STR_INIT_STATE, str_init_state).
-define(STR_BODY_STATE, str_body_state).
-define(STR_ESCSEQ_STATE, str_escseq_state).
-define(STR_FINAL_STATE, str_final_state).
%% whitespace token
-define(WSPACE_INIT_STATE, space_init_state).
-define(WSPACE_BODY_STATE, space_body_state).

%% ====================================================================
%% API functions
%% ====================================================================

-spec create(SkipWhitespaces :: boolean()) -> #lex_analyzer_config{}.
create(SkipWhitespaces) ->
    ParsersConfig = [create_space_config(),
                     create_string_config(),
                     create_word_config()],
    #lex_analyzer_config{token_parsers_config = ParsersConfig, skip_whitespaces = SkipWhitespaces}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec create_word_config() -> #token_parser_config{}.
create_word_config() ->
    TransitionTable = [#transition{from_state = ?WORD_INIT_STATE,
                                   char_predicate = fun word_body_predicate/1,
                                   to_state = ?WORD_BODY_STATE,
                                   char_appender = fun(Char, Buffer) -> [Char] ++ Buffer end},
                       #transition{from_state = ?WORD_BODY_STATE,
                                   char_predicate = fun word_body_predicate/1,
                                   to_state = ?WORD_BODY_STATE,
                                   char_appender = fun(Char, Buffer) -> [Char] ++ Buffer end},
                       #transition{from_state = ?WORD_BODY_STATE,
                                   char_predicate = fun(Char) -> Char == ?EOF_CHAR end,
                                   to_state = ?WORD_BODY_STATE,
                                   char_appender = fun(_Char, Buffer) -> Buffer end}],
    FinalStates = [?WORD_BODY_STATE],
    TokenFactory = fun(#token_parser_state{current_state = ?WORD_BODY_STATE, recognized_buffer = Buffer}) ->
        ?WORD_TOKEN(lists:reverse(Buffer))
    end,
    #token_parser_config{init_state = ?WORD_INIT_STATE,
                         transitions = TransitionTable,
                         final_states = FinalStates,
                         token_factory = TokenFactory}.

-spec create_string_config() -> #token_parser_config{}.
create_string_config() ->
    TransitionTable = [#transition{from_state = ?STR_INIT_STATE,
                                   char_predicate = fun(Char) -> Char == $" end,
                                   to_state = ?STR_BODY_STATE,
                                   char_appender = fun(_Char, Buffer) -> Buffer end},
                       #transition{from_state = ?STR_BODY_STATE,
                                   char_predicate = fun(Char) -> not lists:member(Char, [$\\, $"]) end,
                                   to_state = ?STR_BODY_STATE,
                                   char_appender = fun(Char, Buffer) -> [Char] ++ Buffer end},
                       #transition{from_state = ?STR_BODY_STATE,
                                   char_predicate = fun(Char) -> Char == $\\ end,
                                   to_state = ?STR_ESCSEQ_STATE,
                                   char_appender = fun(_Char, Buffer) -> Buffer end},
                       #transition{from_state = ?STR_ESCSEQ_STATE,
                                   char_predicate = fun(Char) -> lists:member(Char, [$\\, $"]) end,
                                   to_state = ?STR_BODY_STATE,
                                   char_appender = fun(Char, Buffer) -> [Char] ++ Buffer end},
                       #transition{from_state = ?STR_BODY_STATE,
                                   char_predicate = fun(Char) -> Char == $" end,
                                   to_state = ?STR_FINAL_STATE,
                                   char_appender = fun(_Char, Buffer) -> Buffer end},
                       #transition{from_state = ?STR_FINAL_STATE,
                                   char_predicate = fun(Char) -> Char == ?EOF_CHAR end,
                                   to_state = ?STR_FINAL_STATE,
                                   char_appender = fun(_Char, Buffer) -> Buffer end}],
    FinalStates = [?STR_FINAL_STATE],
    TokenFactory = fun(#token_parser_state{current_state = ?STR_FINAL_STATE, recognized_buffer = Buffer}) ->
        ?STRING_TOKEN(lists:reverse(Buffer))
    end,
    #token_parser_config{init_state = ?STR_INIT_STATE,
                         transitions = TransitionTable,
                         final_states = FinalStates,
                         token_factory = TokenFactory}.

-spec create_space_config() -> #token_parser_config{}.
create_space_config() ->
    TransitionTable = [#transition{from_state = ?WSPACE_INIT_STATE,
                                   char_predicate = fun(Char) -> lists:member(Char, " \t") end,
                                   to_state = ?WSPACE_BODY_STATE,
                                   char_appender = fun(_Char, Buffer) -> Buffer end},
                       #transition{from_state = ?WSPACE_BODY_STATE,
                                   char_predicate = fun(Char) -> lists:member(Char, " \t") end,
                                   to_state = ?WSPACE_BODY_STATE,
                                   char_appender = fun(_Char, Buffer) -> Buffer end},
                       #transition{from_state = ?WSPACE_BODY_STATE,
                                   char_predicate = fun(Char) -> Char == ?EOF_CHAR end,
                                   to_state = ?WSPACE_BODY_STATE,
                                   char_appender = fun(_Char, Buffer) -> Buffer end}],
    FinalStates = [?WSPACE_BODY_STATE],
    TokenFactory = fun(#token_parser_state{current_state = ?WSPACE_BODY_STATE}) ->
        ?WHITESPACE_TOKEN
    end,
    #token_parser_config{init_state = ?WSPACE_INIT_STATE,
                         transitions = TransitionTable,
                         final_states = FinalStates,
                         token_factory = TokenFactory}.

-spec word_body_predicate(Char :: char()) -> boolean().
word_body_predicate(Char) ->
    char_category:is_letter(Char) orelse
    char_category:is_digit(Char) orelse
    lists:member(Char, "-_.,/").