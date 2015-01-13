%% @author stdstring

-module(novlan_command).

-behaviour(command_behaviour).

-export([get_name/0, get_command_body/0, get_help/0, execute/4]).

%% TODO (std_string) : move into common place
-define(BAD_ARGS_MESSAGE, "Bad arguments").
-define(BAD_ARGS_CODE, 255).

%% ====================================================================
%% API functions
%% ====================================================================

get_name() -> novlan.

get_command_body() -> ["no", "vlan"].

get_help() -> "no vlan {vlan-list} command help".

execute(Args, _Stdout, Stderr, ExecContext) ->
    case check_args(Args) of
        false -> command_utils:process_error(Stderr, ?BAD_ARGS_MESSAGE, ?BAD_ARGS_CODE, ExecContext);
        true -> {0, ExecContext}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec check_args(Args :: [term()]) -> boolean().
check_args([]) -> false;
check_args(_Other) -> true.