%% @author std-string

-module(name_command).

-behaviour(command_behaviour).

-export([get_name/0, get_command_body/0, get_help/0, execute/4]).

%% TODO (std_string) : move into common place
-define(BAD_ARGS_MESSAGE, "Bad arguments\n").
-define(BAD_ARGS_CODE, 255).

%% ====================================================================
%% API functions
%% ====================================================================

get_name() -> name.

get_command_body() -> ["name"].

get_help() -> "name {name-string} command help".

execute(Args, _Stdout, Stderr, ExecContext) ->
    case check_args(Args) of
        false -> command_utils:process_error(Stderr, ?BAD_ARGS_MESSAGE, ?BAD_ARGS_CODE, ExecContext);
        true -> {0, ExecContext}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec check_args(Args :: [term()]) -> boolean().
check_args([_Name]) -> true;
check_args(_Other) -> false.