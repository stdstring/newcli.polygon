%% @author stdstring

-module(no_name_vlan_command).

-behaviour(command_behaviour).
-behaviour(gen_server).

-include("command_defs.hrl").

-define(COMMAND, no_name_vlan_command).

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_name/0, get_command_body/0, get_help/0, create/3, execute/1]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

get_name() -> no_name_vlan.

get_command_body() -> ["no", "name"].

get_help() -> "no name command".

create(CommandLineRest, Stdout, Stderr) ->
    case check_command(CommandLineRest) of
        false -> {?COMMAND, bad_args};
        true -> start_command(Stdout, Stderr)
    end.

execute(Command) ->
    gen_server:call(Command, execute).

init({Stdout, Stderr}) ->
    State = #command_state{command_line_rest = "", stdout = Stdout, stderr = Stderr},
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

-spec check_command(CommandLineRest :: string()) -> boolean().
check_command(CommandLineRest) ->
    CommandLineRest == "".

-spec start_command(Stdout :: pid(), Stderr  :: pid()) -> pid() | {'no_name_vlan_command', Error :: term()}.
start_command(Stdout, Stderr) ->
    case gen_server:start_link(?MODULE, {Stdout, Stderr}, []) of
        {ok, Pid} -> Pid;
        {error, Error} -> {?COMMAND, Error}
    end.
