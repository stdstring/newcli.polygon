%% @author stdstring

-module(cli_fsm).

-behaviour(gen_event).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1, process_command/1, get_statename/0, get_available_commands/0]).
%% gen_fsm
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

start(_Config) -> error(not_implemented).

process_command(_CommandName) -> error(not_implemented).

get_statename() -> error(not_implemented).

get_available_commands() -> error(not_implemented).

init(_Args) -> error(not_implemented).

handle_event(_Event, _State) -> error(not_implemented).

handle_call(_Request, _State) -> error(not_implemented).

handle_info(_Info, _State) -> error(not_implemented).

terminate(_Arg, _State) -> true.

code_change(_OldVsn, _State, _Extra) -> error(not_implemented).

%% ====================================================================
%% Internal functions
%% ====================================================================

