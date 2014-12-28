%% @author std-string

-module(prompt_factory).

-export([generate_prompt/1]).

-include("authentication_defs.hrl").
-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec generate_prompt(ExecutionState :: #client_handler_state{}) -> string().
generate_prompt(_State) ->
    "".

%% ====================================================================
%% Internal functions
%% ====================================================================