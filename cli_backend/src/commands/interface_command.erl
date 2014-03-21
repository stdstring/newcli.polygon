%% @author stdstring

-module(interface_command).

-behaviour(command_behaviour).
-behaviour(gen_server).

-include("command_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_name/0, get_command_body/0, get_help/0, create/3, execute/1]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

get_name() -> interface.

get_command_body() -> ["interface"].

get_help() -> "interface {interface-id} command".

create(CommandLineParts, Stdout, Stderr) ->
    case check_command(CommandLineParts) of
        false -> {interface_command, bad_args};
        true -> start_command(CommandLineParts, Stdout, Stderr)
    end.

execute(Command) ->
    gen_server:call(Command, execute).

init({CommandLineParts, Stdout, Stderr}) ->
    State = #command_state{command_line = CommandLineParts, stdout = Stdout, stderr = Stderr},
    {ok, State}.

handle_call(execute, _From, State) ->
    {reply, 0, State}.

handle_cast(_Request, _State) -> error(not_supported).

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> true.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec check_command(CommandLineParts :: [string()]) -> boolean().
check_command(CommandLineParts) ->
    CommandBody = get_command_body(),
    lists:prefix(CommandBody, CommandLineParts) andalso length(CommandLineParts) == (length(CommandBody) + 1).

-spec start_command(CommandLineParts :: [string()], Stdout :: pid(), Stderr  :: pid()) -> pid() | {'config_terminal_command', Error :: term()}.
start_command(CommandLineParts, Stdout, Stderr) ->
    case gen_server:start_link(?MODULE, {CommandLineParts, Stdout, Stderr}, []) of
        {ok, Pid} -> Pid;
        {error, Error} -> {interface_command, Error}
    end.