%% @author std-string

-module(command_parser).

-export([process/3]).

-include("lexical_defs.hrl").
-include("syntax_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec process(Source :: string(),
              LexConfig :: #lex_analyzer_config{},
              SyntaxConfig :: #syntax_analyzer_config{}) ->
    {'true', Result :: term()} | {'false', Reason :: term()}.
process(Source, LexConfig, SyntaxConfig) ->
    case lex_analyzer:process(Source, LexConfig) of
        {true, TokenList} ->
            case syntax_analyzer:process(TokenList, SyntaxConfig) of
                {true, Result} -> {true, Result};
                {false, Reason} -> {false, Reason}
            end;
        {false, Reason} -> {false, Reason}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================