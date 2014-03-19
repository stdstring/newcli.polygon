%% @author std-string

-module(commandline_parser).

%% ====================================================================
%% API functions
%% ====================================================================

-export([parse/1]).

-spec parse(CommandLine :: string()) -> [string()].
parse(CommandLine) ->
    string:tokens(CommandLine, " ").

%% ====================================================================
%% Internal functions
%% ====================================================================

