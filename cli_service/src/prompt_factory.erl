%% @author std-string

-module(prompt_factory).

-export([generate_prompt/1]).

-include("authentication_defs.hrl").
-include("common_defs.hrl").
-include("cli_fsm_defs.hrl").

-define(ADMIN_FOOTER, "#").
-define(USER_FOOTER, ">").
-define(PROMPT_FORMAT, "~s@~s~s~s").

%% ====================================================================
%% API functions
%% ====================================================================

-spec generate_prompt(State :: #client_handler_state{}) -> string().
generate_prompt(State) ->
    User = State#client_handler_state.user,
    DeviceName = State#client_handler_state.config#global_config.device_name,
    CliFsm = State#client_handler_state.cli_fsm,
    CliFsmState = cli_fsm:get_current_state(CliFsm),
    CurrentStateRepr = CliFsmState#cli_fsm_state_info.current_state_representation,
    generate_prompt(User, DeviceName, CurrentStateRepr).

%% ====================================================================
%% Internal functions
%% ====================================================================

generate_prompt(#user{username = Username, access_level = ?MAX_ACCESS_LEVEL}, DeviceName, CurrentStateRepr) ->
    string_utils:format(?PROMPT_FORMAT, [Username, DeviceName, CurrentStateRepr, ?ADMIN_FOOTER]);
generate_prompt(#user{username = Username}, DeviceName, CurrentStateRepr) ->
    string_utils:format(?PROMPT_FORMAT, [Username, DeviceName, CurrentStateRepr, ?USER_FOOTER]);
generate_prompt(undefined, DeviceName, CurrentStateRepr) ->
    string_utils:format(?PROMPT_FORMAT, ["", DeviceName, CurrentStateRepr, ?USER_FOOTER]).