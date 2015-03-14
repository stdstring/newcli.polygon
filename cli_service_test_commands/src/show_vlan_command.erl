%% @author std-string

-module(show_vlan_command).

-behaviour(command_behaviour).

-export([get_name/0, get_command_body/0, get_help/0, execute/4]).

%% TODO (std_string) : move into common place
-define(BAD_ARGS_MESSAGE, "Bad arguments\n").
-define(BAD_ARGS_CODE, 255).

%% ====================================================================
%% API functions
%% ====================================================================

get_name() -> show_vlan.

get_command_body() -> ["show", "vlan"].

get_help() -> "show vlan command help".

execute(Args, Stdout, Stderr, ExecContext) ->
    case check_args(Args) of
        false -> command_utils:process_error(Stderr, ?BAD_ARGS_MESSAGE, ?BAD_ARGS_CODE, ExecContext);
        true -> process_command(Stdout, ExecContext)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec check_args(Args :: [term()]) -> boolean().
check_args([]) -> true;
check_args(_Other) -> false.

-spec process_command(Stdout :: pid(), ExecContext :: [{Key :: atom(), Value :: term()}]) ->
    {ReturnValue :: integer(), ExecContext :: [{Key :: atom(), Value :: term()}]}.
process_command(Stdout, ExecContext) ->
    command_utils:send_output(Stdout, "show vlan line 1\n"),
    command_utils:send_output(Stdout, "show vlan line 2\n"),
    command_utils:send_output(Stdout, "show vlan line 3\n"),
    {0, ExecContext}.