%% @author std-string

-module(string_utils).

-export([format/2]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec format(Format :: string(), Data :: [term()]) -> string().
format(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

%% ====================================================================
%% Internal functions
%% ====================================================================