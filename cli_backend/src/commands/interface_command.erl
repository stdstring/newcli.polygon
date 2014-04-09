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

create(CommandLineRest, Stdout, Stderr) ->
    case check_command(CommandLineRest) of
        false -> {error, bad_args};
        true -> gen_server:start_link(?MODULE, {CommandLineRest, Stdout, Stderr}, [])
    end.

execute(Command) ->
    gen_server:call(Command, execute).

init({CommandLineRest, Stdout, Stderr}) ->
    State = #command_state{command_line_rest = CommandLineRest, stdout = Stdout, stderr = Stderr},
    {ok, State}.

handle_call(execute, _From, State) ->
    {reply, 0, State}.

handle_cast(_Request, State) -> {stop, enotsup, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> true.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec check_command(CommandLineRest :: string()) -> boolean().
check_command(CommandLineRest) ->
    CommandLineRest /= "".
