%% @author std-string

-module(cli_terminal_listen_supervisor).

-behaviour(supervisor).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start_link/1]).
%% supervisor behaviour
-export([init/1]).

start_link(_ListenSocketConfig) -> ok.

init(ListenSocketConfig) ->
    ChildSpecs =
        [{cli_terminal_listen, {cli_terminal_listen_endpoint, start_link, [ListenSocketConfig]}, transient, brutal_kill, worker, [cli_terminal_listen_endpoint]}],
    {ok, {{one_for_one, 1, 60}, ChildSpecs}}.

%% ====================================================================
%% Internal functions
%% ====================================================================