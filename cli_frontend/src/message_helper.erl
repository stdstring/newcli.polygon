%% @author stdstring

-module(message_helper).

%% ====================================================================
%% API functions
%% ====================================================================

-export([format/2]).

-spec format(Format :: string(), Data :: [term()]) -> string().
format(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

%% ====================================================================
%% Internal functions
%% ====================================================================