%% @author std-string

-module(string_utils).

-export([format/2, get_common_prefix/1, get_common_prefix/2]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec format(Format :: string(), Data :: [term()]) -> string().
format(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

-spec get_common_prefix(Strings :: [string()]) -> string().
get_common_prefix([]) -> "";
get_common_prefix([FirstStr]) -> FirstStr;
get_common_prefix([FirstStr | Rest]) ->
    AggrFun = fun(Str, Prefix) -> get_common_prefix(Str, Prefix) end,
    lists:foldl(AggrFun, FirstStr, Rest).

-spec get_common_prefix(Str1 :: string(), Str2 :: string()) -> string().
get_common_prefix(Str1, Str2) -> get_common_prefix(Str1, Str2, "").

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec get_common_prefix(Str1 :: string(), Str2 :: string(), Prefix :: string()) -> string().
get_common_prefix([Char | Rest1], [Char | Rest2], Prefix) ->
    get_common_prefix(Rest1, Rest2, [Char] ++ Prefix);
get_common_prefix(_Str1, _Str2, Prefix) ->
    lists:reverse(Prefix).