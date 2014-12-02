%% @author std-string

-module(lex_analyzer_config).

-export([create/1]).

-include("lexical_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec create(SkipWhitespaces :: boolean()) -> #lex_analyzer_config{}.
create(_SkipWhitespaces) ->
    #lex_analyzer_config{}.

%% ====================================================================
%% Internal functions
%% ====================================================================