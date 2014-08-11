%% @author std-string

-module(cli_terminal_supervisor).

-behaviour(supervisor).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start_link/1]).
%% supervisor behaviour
-export([init/1]).

start_link(_SocketConfig) -> ok.

init(_SocketConfig) -> ok.

%% ====================================================================
%% Internal functions
%% ====================================================================