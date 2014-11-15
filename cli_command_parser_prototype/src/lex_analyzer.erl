%% @author std-string

-module(lex_analyzer).

-export([parse/2]).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec parse(Source :: string(), ConfigList :: [#token_parser_config{}]) ->
    {'true', TokenList :: [#token{}]} | {'false', Reason :: term()}.
parse(_Source, _ConfigList) -> ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

%%-spec init_parser_list(ConfigList :: [#token_parser_config{}]) -> [{ParserConfig :: #token_parser_config{}, State :: atom()}].
%%init_parser_list(ConfigList) ->
%%    lists:map(fun(Config) -> {Config, Config#token_parser_config.init_state} end, ConfigList).

%%-spec process_char(Char :: byte(), _ActiveParserList :: [{ParserConfig :: #token_parser_config{}, State :: atom()}]) -> 'ok'.
%%process_char(_Char, _ActiveParserList) -> ok.