%% @author std-string

-module(commandline_parser).

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_first_token/1]).

-spec get_first_token(Source :: string()) -> {Tokens :: [string()], Rest :: string()}.
get_first_token(Source) ->
    %% 32 == space ascii code
    lists:splitwith(fun(Character) -> Character /= 32 end, Source).

%% ====================================================================
%% Internal functions
%% ====================================================================

