%% @author std-string

-module(lex_analyzer_tests).

-include_lib("eunit/include/eunit.hrl").

-include("common_defs.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

parse_test() ->
    ConfigList = create_config(),
    io:format(user, "ConfigList: ~p~n", [ConfigList]),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

create_word_config() ->
    #token_parser_config{}.

create_string_config() ->
    #token_parser_config{}.

create_config() ->
    [create_word_config(), create_string_config()].