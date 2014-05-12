%% @author std-string

-module(commandline_parser).

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_first_token/1, get_first_token/2, get_tokens/1, get_tokens/2]).

-spec get_first_token(Source :: string()) -> {Tokens :: [string()], Rest :: string()}.
get_first_token(Source) ->
    %% $\s == space character
    get_first_token(Source, $\s).

-spec get_first_token(Source :: string(), Delimiter :: char()) -> {Tokens :: [string()], Rest :: string()}.
get_first_token(Source, Delimiter) ->
    {Token, Rest} = lists:splitwith(fun(Character) -> Character /= Delimiter end, Source),
    {Token, string:strip(Rest, left, Delimiter)}.

-spec get_tokens(Source :: string()) -> [string()].
get_tokens(Source) ->
    %% delimiter == space
    get_tokens(Source, " ").

-spec get_tokens(Source :: string(), Delimiter :: string()) -> [string()].
get_tokens(Source, Delimiter) ->
    string:tokens(Source, Delimiter).

%% ====================================================================
%% Internal functions
%% ====================================================================

