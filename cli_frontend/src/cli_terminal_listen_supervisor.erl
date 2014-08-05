%% @author std-string

-module(cli_terminal_listen_supervisor).

-behaviour(supervisor).

-define(SUPERVISOR_NAME, cli_terminal_listen_supervisor).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start_link/1]).
%% supervisor behaviour
-export([init/1]).

start_link(_ListenSocketConfig) -> ok.

init(_ListenSocketConfig) -> ok.

%% ====================================================================
%% Internal functions
%% ====================================================================