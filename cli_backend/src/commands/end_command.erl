%% @author stdstring

-module(end_command).

-behaviour(command_behaviour).
-behaviour(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_name/0, get_command_body/0, get_help/0, create/3, execute/1]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

get_name() -> 'end'.

get_command_body() -> ["end"].

get_help() -> "end command".

create(CommandLineParts, Stdout, Stderr) -> error(not_implemented).

execute(Command) -> error(not_implemented).

init(_Args) -> error(not_implemented).

handle_call(_Request, _From, _State) -> error(not_implemented).

handle_cast(_Request, _State) -> error(not_implemented).

handle_info(_Request, _Info) -> error(not_supported).

terminate(_Reason, _State) -> true.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

