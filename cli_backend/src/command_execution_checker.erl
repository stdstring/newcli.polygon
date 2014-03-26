%% @author std-string

-module(command_execution_checker).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([execution_precheck/3]).

-spec execution_precheck(CommandModule :: atom(), CliFsm :: pid(), User :: #user{}) -> boolean().
execution_precheck(CommandModule, CliFsm, User) ->
    CommandName = apply(CommandModule, get_name, []),
    case authorization_service:authorize(User, CommandName) of
        {authorization_result, access_allowed} ->
            {_CurrentState, Commands} = cli_fsm:get_current_state(CliFsm),
            lists:member(CommandName, Commands);
        {authorization_result, access_denied} -> false;
        {authorization_fail, unknown_command} -> false
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

