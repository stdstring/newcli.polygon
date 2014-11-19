-module(command_parser_tests).

-include_lib("eunit/include/eunit.hrl").

-include("common_defs.hrl").
-include("token_defs.hrl").

%% for lex analyzer
-define(WORD_INIT_STATE, word_init_state).
-define(WORD_BODY_STATE, word_body_state).
-define(STR_INIT_STATE, str_init_state).
-define(STR_BODY_STATE, str_body_state).
-define(STR_FINAL_STATE, str_final_state).
-define(SPACE_INIT_STATE, space_init_state).
-define(SPACE_BODY_STATE, space_body_state).

%% ====================================================================
%% Test functions
%% ====================================================================

parse_test() ->
    LexConfig = lex_analyzer_config:create_config(),
    io:format(user, "~p~n", [LexConfig]),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================