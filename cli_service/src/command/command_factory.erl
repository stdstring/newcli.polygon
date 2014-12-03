%% @author std-string

-module(command_factory).

-export([process/3]).

%% ====================================================================
%% API functions
%% ====================================================================

process(Source, LexConfig, SyntaxConfig) ->
    case command_parser:process(Source, LexConfig, SyntaxConfig) of
        {true, Result} -> {true, Result};
        {false, Reason} -> {false, Reason}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================