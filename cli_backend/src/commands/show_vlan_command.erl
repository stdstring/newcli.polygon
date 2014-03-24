%% @author stdstring

-module(show_vlan_command).

-behaviour(command_behaviour).
-behaviour(gen_server).

-behaviour(command_behaviour).
-behaviour(gen_server).

-include("message_defs.hrl").
-include("command_defs.hrl").

-define(COMMAND, show_vlan_command).

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_name/0, get_command_body/0, get_help/0, create/3, execute/1]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

get_name() -> show_vlan.

get_command_body() -> ["show", "vlan"].

get_help() -> "show vlan command".

create(CommandLineRest, Stdout, Stderr) -> error(not_implemented).

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

