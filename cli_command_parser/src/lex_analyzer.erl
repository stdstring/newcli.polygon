%% @author std-string

-module(lex_analyzer).

-export([process/2]).

-include("lexical_defs.hrl").
-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec process(Source :: string(), Config :: #lex_analyzer_config{}) ->
    {'true', TokenList :: [#token{}]} | {'false', Reason :: term()}.
process(Source, Config) ->
    case process_string(Source, Config) of
        {true, TokenList} ->
            {true, lists:reverse([?END_TOKEN] ++ TokenList)};
        {false, Reason} -> {false, Reason}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_string(Source :: string(), Config :: #lex_analyzer_config{}) ->
    {true, TokenList :: [#token{}]} | {false, Reason :: term()}.
process_string(Source, Config) ->
    ParserList = init_parser_list(Config#lex_analyzer_config.token_parsers_config),
    process_string(Source, Config, ParserList, {undefined, []}, []).

-spec process_string(Source :: string(),
                     Config :: #lex_analyzer_config{},
                     ParserList :: [{ParserConfig :: #token_parser_config{}, State :: #token_parser_state{}}],
                     BestToken :: {Token :: #token{}, Rest :: [byte()]},
                     TokenList :: [#token{}]) ->
    {true, TokenList :: [#token{}]} | {false, Reason :: term()}.
process_string([], Config, ParserList, {undefined, []}, TokenList) ->
    process_eof(Config, ParserList, {undefined, []}, TokenList);
process_string([], Config, ParserList, {Token, []}, TokenList) ->
    process_eof(Config, ParserList, {Token, []}, TokenList);
process_string([], Config, ParserList, {Token, Rest}, TokenList) ->
    process_eof(Config, ParserList, {Token, Rest}, TokenList);
process_string([Char | Rest], Config, ParserList, BestToken, TokenList) ->
    case process_char(Char, ParserList) of
        {[], undefined} ->
            case BestToken of
                {undefined, []} -> {false, unsuitable_char};
                {Token, TokenRest} -> restart_process_string(Config, Token, TokenRest, TokenList)
            end;
        {NewParserList, undefined} ->
            process_string(Rest, Config, NewParserList, BestToken, TokenList);
        {NewParserList, NewToken} ->
            NewBestToken = {NewToken, Rest},
            process_string(Rest, Config, NewParserList, NewBestToken, TokenList)
    end.

-spec restart_process_string(Config :: #lex_analyzer_config{},
                             Token :: #token{},
                             TokenRest :: string(),
                             TokenList :: [#token{}]) -> no_return().
restart_process_string(Config, Token, TokenRest, TokenList) ->
    NewParserList = init_parser_list(Config#lex_analyzer_config.token_parsers_config),
    NewTokenList = append_token(Token, TokenList, Config#lex_analyzer_config.skip_whitespaces),
    process_string(TokenRest, Config, NewParserList, {undefined, []}, NewTokenList).

-spec init_parser_list(ConfigList :: [#token_parser_config{}]) ->
    [{ParserConfig :: #token_parser_config{}, State :: #token_parser_state{}}].
init_parser_list(ConfigList) ->
    MapFun = fun(Config) ->
        {Config, #token_parser_state{current_state = Config#token_parser_config.init_state}}
    end,
    lists:map(MapFun, ConfigList).

-spec process_char(Char :: char(),
                   ParserList :: [{ParserConfig :: #token_parser_config{}, State :: #token_parser_state{}}]) ->
    {ParserList :: [{ParserConfig :: #token_parser_config{}, State :: #token_parser_state{}}], Token :: 'undefined' | #token{}}.
process_char(Char, ParserList) -> process_char(Char, ParserList, [], undefined).

-spec process_char(Char :: char(),
                   ParserList :: [{ParserConfig :: #token_parser_config{}, State :: #token_parser_state{}}],
                   NextParserList :: [{ParserConfig :: #token_parser_config{}, State :: #token_parser_state{}}],
                   Token :: 'undefined' | #token{}) ->
    {ParserList :: [{ParserConfig :: #token_parser_config{}, State :: #token_parser_state{}}], Token :: 'undefined' | #token{}}.
process_char(_Char, [], NextParserList, Token) ->
    {lists:reverse(NextParserList), Token};
process_char(Char, [{Config, State} | ParserRest], NextParserList, Token) ->
    case token_parser:process(State, Char, Config) of
        {true, #token_parser_result{token = undefined, state = NewState}} ->
            process_char(Char, ParserRest, [{Config, NewState}] ++ NextParserList, Token);
        {true, #token_parser_result{token = NewToken, state = NewState}} ->
            process_char(Char, ParserRest, [{Config, NewState}] ++ NextParserList, NewToken);
        false ->
            process_char(Char, ParserRest, NextParserList, Token)
    end.

-spec append_token(Token :: #token{}, TokenList :: [#token{}], SkipWhitespaces :: boolean()) -> [#token{}].
append_token(?WHITESPACE_TOKEN, TokenList, true) -> TokenList;
append_token(Token, TokenList, _SkipWhitespaces) -> [Token] ++ TokenList.

-spec process_eof(Config :: #lex_analyzer_config{},
                  ParserList :: [{ParserConfig :: #token_parser_config{}, State :: #token_parser_state{}}],
                  Token :: #token{},
                  TokenList :: [#token{}]) ->
    {true, TokenList :: [#token{}]} | {false, Reason :: term()} | no_return().
process_eof(Config, ParserList, Token, TokenList) ->
    case process_char(?EOF_CHAR, ParserList) of
        {_NewParserList, undefined} ->
            case Token of
                {undefined, []} ->
                    {false, bad_input};
                {Token, []} ->
                    NewTokenList = append_token(Token, TokenList, Config#lex_analyzer_config.skip_whitespaces),
                    {true, NewTokenList};
                {Token, Rest} ->
                    restart_process_string(Config, Token, Rest, TokenList)
            end;
        {_NewParserList, NewToken} ->
            NewTokenList = append_token(NewToken, TokenList, Config#lex_analyzer_config.skip_whitespaces),
            {true, NewTokenList}
    end.