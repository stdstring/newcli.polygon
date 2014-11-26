%% @author std-string

-module(command_parser).

-export([process/2]).

-include("lexical_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec process(Source :: string(),
              LexConfig :: #lex_analyzer_config{}) -> 'ok'.
process(_Source, _LexConfig) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================