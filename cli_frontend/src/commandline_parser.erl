%% @author std-string

-module(commandline_parser).

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_first_token/1, get_first_token/2]).

-spec get_first_token(Source :: string()) -> {Tokens :: [string()], Rest :: string()}.
get_first_token(Source) ->
    %% 32 == space ascii code
    get_first_token(Source, 32).

-spec get_first_token(Source :: string(), Delimiter :: char()) -> {Tokens :: [string()], Rest :: string()}.
get_first_token(Source, Delimiter) ->
    {Token, Rest} = lists:splitwith(fun(Character) -> Character /= Delimiter end, Source),
    {Token, string:strip(Rest, left, Delimiter)}.


%% ====================================================================
%% Internal functions
%% ====================================================================

