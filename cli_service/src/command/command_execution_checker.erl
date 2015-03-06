%% @author std-string

-module(command_execution_checker).

-include("authentication_defs.hrl").
-include("cli_fsm_defs.hrl").

-export([execution_precheck/3, select_suitable_commands/2]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec execution_precheck(CommandName :: atom(), CliFsm :: pid(), User :: #user{} | 'undefined') ->
    'true' | {'false', 'access_denied'} | {'false', 'authorization_bad_config'} | {'false', 'unsuitable_command'}.
execution_precheck(CommandName, CliFsm, User) ->
    case authorization_service:authorize_command(User, CommandName) of
        {authorization_result, access_allowed} -> execution_cli_precheck(CommandName, CliFsm);
        {authorization_result, access_denied} -> {false, access_denied};
        {authorization_fail, unknown_command} -> {false, authorization_bad_config}
    end.

-spec select_suitable_commands(CliFsm :: pid(), User :: #user{} | 'undefined') -> [CommandName :: atom()].
select_suitable_commands(CliFsm, User) ->
    #cli_fsm_state_info{commands = Commands} = cli_fsm:get_current_state(CliFsm),
    authorization_service:authorize_commands(User, Commands).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec execution_cli_precheck(CommandName :: atom(), CliFsm :: pid()) -> 'true' | {'false', Reason :: atom()}.
execution_cli_precheck(CommandName, CliFsm) ->
    #cli_fsm_state_info{commands = Commands} = cli_fsm:get_current_state(CliFsm),
    case lists:member(CommandName, Commands) of
        true -> true;
        false -> {false, unsuitable_command}
    end.