%% @author std-string

-module(lex_analyzer).

-export([parse/3]).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec parse(Source :: string(), ConfigList :: [#token_parser_config{}], SkipWhitespaces :: boolean()) ->
    {'true', TokenList :: [#token{}]} | {'false', Reason :: term()}.
parse(Source, ConfigList, SkipWhitespaces) ->
    case process_string(Source, ConfigList) of
        {true, TokenList} ->
            {true, lists:reverse([#token{type = 'end', value = ""}] ++ filter_tokens(TokenList, SkipWhitespaces))};
        {false, Reason} -> {false, Reason}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec filter_tokens(TokenList :: [#token{}], SkipWhitespaces :: boolean()) -> [#token{}].
filter_tokens(TokenList, false) -> TokenList;
filter_tokens(TokenList, true) ->
    lists:filter(fun(#token{type = Type}) -> Type /= whitespace end, TokenList).

-spec process_string(Source :: [byte()], ConfigList :: [#token_parser_config{}]) ->
    {true, TokenList :: [#token{}]} | {false, Reason :: term()}.
process_string(Source, ConfigList) ->
    ParserList = init_parser_list(ConfigList),
    process_string(Source, ConfigList, ParserList, {undefined, []}, []).

-spec process_string(Source :: [byte()],
                     ConfigList :: [#token_parser_config{}],
                     ParserList :: [{ParserConfig :: #token_parser_config{}, State :: #token_parser_state{}}],
                     BestToken :: {Token :: #token{}, Rest :: [byte()]},
                     TokenList :: [#token{}]) ->
    {true, TokenList :: [#token{}]} | {false, Reason :: term()}.
process_string([], _ConfigList, _ParserList, {undefined, []}, _TokenList) ->
    {false, bad_input};
process_string([], _ConfigList, _ParserList, {Token, []}, TokenList) ->
    {true, [Token] ++ TokenList};
process_string([], _ConfigList, _ParserList, {_Token, _Rest}, _TokenList) ->
    {false, bad_input};
process_string(_Source, _ConfigList, [], {undefined, []}, _TokenList) ->
    {false, unsuitable_char};
process_string(_Source, ConfigList, [], {Token, TokenRest}, TokenList) ->
    ParserList = init_parser_list(ConfigList),
    process_string(TokenRest, ConfigList, ParserList, {undefined, []}, [Token] ++ TokenList);
process_string([Char | Rest], ConfigList, ParserList, BestToken, TokenList) ->
    case process_char(Char, ParserList) of
        {NewParserList, undefined} ->
            process_string(Rest, ConfigList, NewParserList, BestToken, TokenList);
        {NewParserList, NewToken} ->
            NewBestToken = {NewToken, Rest},
            process_string(Rest, ConfigList, NewParserList, NewBestToken, TokenList)
    end.

-spec init_parser_list(ConfigList :: [#token_parser_config{}]) ->
    [{ParserConfig :: #token_parser_config{}, State :: #token_parser_state{}}].
init_parser_list(ConfigList) ->
    MapFun = fun(Config) ->
        {Config, #token_parser_state{current_state = Config#token_parser_config.init_state}}
    end,
    lists:map(MapFun, ConfigList).

-spec process_char(Char :: byte(),
                   ParserList :: [{ParserConfig :: #token_parser_config{}, State :: #token_parser_state{}}]) ->
    {ParserList :: [{ParserConfig :: #token_parser_config{}, State :: #token_parser_state{}}], Token :: 'undefined' | #token{}}.
process_char(Char, ParserList) -> process_char(Char, ParserList, [], undefined).

-spec process_char(Char :: byte(),
                   ParserList :: [{ParserConfig :: #token_parser_config{}, State :: #token_parser_state{}}],
                   NextParserList :: [{ParserConfig :: #token_parser_config{}, State :: #token_parser_state{}}],
                   Token :: 'undefined' | #token{}) ->
    {ParserList :: [{ParserConfig :: #token_parser_config{}, State :: #token_parser_state{}}], Token :: 'undefined' | #token{}}.
process_char(_Char, [], NextParserList, Token) ->
    {NextParserList, Token};
process_char(Char, [{Config, State} | ParserRest], NextParserList, Token) ->
    case token_parser:parse(State, Char, Config) of
        {true, #token_parser_result{token = undefined, state = NewState}} ->
            process_char(Char, ParserRest, NextParserList ++ [{Config, NewState}], Token);
        {true, #token_parser_result{token = NewToken, state = NewState}} ->
            process_char(Char, ParserRest, NextParserList ++ [{Config, NewState}], NewToken);
        false ->
            process_char(Char, ParserRest, NextParserList, Token)
    end.