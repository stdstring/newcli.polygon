%% @author std-string

-module(execution_context_factory).

-include("authentication_defs.hrl").
-include("command_defs.hrl").
-include("common_defs.hrl").

-export([create/1]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec create(State :: #client_handler_state{}) -> [{Key :: atom(), Value :: term()}].
create(#client_handler_state{user = User}) ->
    [{?USER_KEY, User}, {?EX_STATE_KEY, ?EX_CONTINUE}].

%% ====================================================================
%% Internal functions
%% ====================================================================