-module(lex_analyzer_config).

-export([create_config/0]).

-include("common_defs.hrl").
-include("token_defs.hrl").

-define(WORD_INIT_STATE, word_init_state).
-define(WORD_BODY_STATE, word_body_state).
-define(STR_INIT_STATE, str_init_state).
-define(STR_BODY_STATE, str_body_state).
-define(STR_FINAL_STATE, str_final_state).
-define(SPACE_INIT_STATE, space_init_state).
-define(SPACE_BODY_STATE, space_body_state).

%% ====================================================================
%% API functions
%% ====================================================================

create_config() ->
    [create_word_config(), create_string_config(), create_space_config()].

%% ====================================================================
%% Internal functions
%% ====================================================================

create_word_config() ->
    TransitionTable = [#transition{from_state = ?WORD_INIT_STATE,
                                   char_predicate = fun(Char) -> word_body_predicate(Char) end,
                                   to_state = ?WORD_BODY_STATE,
                                   char_appender = fun(Char, Buffer) -> [Char] ++ Buffer end},
                       #transition{from_state = ?WORD_BODY_STATE,
                                   char_predicate = fun(Char) -> word_body_predicate(Char) end,
                                   to_state = ?WORD_BODY_STATE,
                                   char_appender = fun(Char, Buffer) -> [Char] ++ Buffer end}],
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
                                   char_predicate = fun(Char) -> Char == $" end,
                                   to_state = ?STR_BODY_STATE,
                                   char_appender = fun(_Char, Buffer) -> Buffer end},
                       #transition{from_state = ?STR_BODY_STATE,
                                   char_predicate = fun(Char) -> Char /= $" end,
                                   to_state = ?STR_BODY_STATE,
                                   char_appender = fun(Char, Buffer) -> [Char] ++ Buffer end},
                       #transition{from_state = ?STR_BODY_STATE,
                                   char_predicate = fun(Char) -> Char == $" end,
                                   to_state = ?STR_FINAL_STATE,
                                   char_appender = fun(_Char, Buffer) -> Buffer end}],
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
                                   char_predicate = fun(Char) -> lists:member(Char, " \t") end,
                                   to_state = ?SPACE_BODY_STATE,
                                   char_appender = fun(_Char, Buffer) -> Buffer end},
                       #transition{from_state = ?SPACE_BODY_STATE,
                                   char_predicate = fun(Char) -> lists:member(Char, " \t") end,
                                   to_state = ?SPACE_BODY_STATE,
                                   char_appender = fun(_Char, Buffer) -> Buffer end}],
    FinalStates = [?SPACE_BODY_STATE],
    TokenFactory = fun(#token_parser_state{current_state = ?SPACE_BODY_STATE}) ->
        #token{type = whitespace, value = ""}
    end,
    #token_parser_config{init_state = ?SPACE_INIT_STATE,
                         transitions = TransitionTable,
                         final_states = FinalStates,
                         token_factory = TokenFactory}.

word_body_predicate(Char) ->
    char_category:is_letter(Char) orelse
    char_category:is_digit(Char) orelse
    lists:member(Char, "\\.,-").