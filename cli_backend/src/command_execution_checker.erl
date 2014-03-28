%% @author std-string

-module(command_execution_checker).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([execution_precheck/3, execution_postcheck/1]).

-spec execution_precheck(CommandModule :: atom(), CliFsm :: pid(), User :: #user{}) -> 'true' | {'false', Reason :: atom()}.
execution_precheck(CommandModule, CliFsm, User) ->
    CommandName = apply(CommandModule, get_name, []),
    case authorization_service:authorize(User, CommandName) of
        {authorization_result, access_allowed} ->
            {_CurrentState, Commands} = cli_fsm:get_current_state(CliFsm),
            case lists:member(CommandName, Commands) of
                true -> true;
                false -> {false, unsuitable_command}
            end;
        {authorization_result, access_denied} -> {false, access_denied};
        {authorization_fail, unknown_command} -> {false, authorization_bad_config}
    end.

execution_postcheck(CliFsm) ->
    #cli_fsm_state_info{is_terminal =IsTerminalState} = cli_fsm:get_current_state(CliFsm),
    case IsTerminalState of
        false -> true;
        true -> {false, cli_terminal_state}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

