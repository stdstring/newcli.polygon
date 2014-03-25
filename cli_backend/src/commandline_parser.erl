%% @author std-string

-module(commandline_parser).

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_first_token/1, get_tokens/2, get_tokens/3]).

-spec get_first_token(Source :: string()) -> {Tokens :: [string()], Rest :: string()}.
get_first_token(Source) ->
        get_tokens(Source, 1).

-spec get_tokens(Source :: string(), Count :: integer()) -> {Tokens :: [string()], Rest :: string()}.
get_tokens(Source, Count) ->
    %% 32 == space ascii code
    get_tokens(Source, 32, Count).

-spec get_tokens(Source :: string(), Delimiter :: integer(), Count :: integer()) -> {Tokens :: [string()], Rest :: string()}.
get_tokens(Source, Delimiter, Count) ->
    get_tokens_impl(Source, Delimiter, Count, []).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec get_tokens_impl(Source :: string(), Delimiter :: char(), Count :: integer(), Tokens :: [string()]) -> {Tokens :: [string()], Rest :: string()}.
get_tokens_impl(Source, _Delimiter, 0, Tokens) -> {lists:reverse(Tokens), Source};
get_tokens_impl("", _Delimiter, _Count, Tokens) -> {lists:reverse(Tokens), ""};
get_tokens_impl(Source, Delimiter, Count, Tokens) ->
    {Token, Rest} = lists:splitwith(fun(Character) -> Character /= Delimiter end, Source),
    get_tokens_impl(string:strip(Rest, left, Delimiter), Delimiter, Count - 1, [Token] ++ Tokens).