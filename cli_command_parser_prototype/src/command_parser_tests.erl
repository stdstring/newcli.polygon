-module(command_parser_tests).

-include_lib("eunit/include/eunit.hrl").

-include("common_defs.hrl").
-include("token_defs.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

parse_test() ->
    LexConfig = lex_analyzer_config:create_config(),
    io:format(user, "~p~n", [LexConfig]),
    NameConfig = name_search_config:create_config(),
    io:format(user, "~p~n", [NameConfig]),
    SyntaxConfig = syntax_analyzer_config:create_config(),
    io:format(user, "~p~n", [SyntaxConfig]),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================