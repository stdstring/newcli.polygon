%% @author stdstring

-module(exit_command).

-behaviour(command_behaviour).
-behaviour(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_name/0, get_command_body/0, get_help/0, create/3, execute/1]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

get_name() -> exit.

get_command_body() -> ["exit"].

get_help() -> "exit command".

create(CommandLineParts, Stdout, Stderr) ->
    case check_command(CommandLineParts) of
        false -> {exit_command, bad_args};
        true -> start_command(CommandLineParts, Stdout, Stderr)
    end.

execute(Command) ->
    gen_server:call(Command, execute).

init(_Args) -> error(not_implemented).

handle_call(_Request, _From, State) ->
    {reply, 0, State}.

handle_cast(_Request, _State) -> error(not_implemented).

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> true.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec check_command(CommandLineParts :: [string()]) -> boolean().
check_command(CommandLineParts) ->
    CommandLineParts == get_command_body().

-spec start_command(CommandLineParts :: [string()], Stdout :: pid(), Stderr  :: pid()) -> pid() | {'exit_command', Error :: term()}.
start_command(CommandLineParts, Stdout, Stderr) ->
    case gen_server:start_link(?MODULE, {CommandLineParts, Stdout, Stderr}, []) of
        {ok, Pid} -> Pid;
        {error, Error} -> {exit_command, Error}
    end.
