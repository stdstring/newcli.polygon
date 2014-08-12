%% @author std-string

-module(cli_terminal_listen_supervisor).

-behaviour(supervisor).

-include("cli_terminal_common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1]).
%% supervisor behaviour
-export([init/1]).

start(Config) ->
    supervisor:start_link({local, ?LISTEN_SUPERVISOR_NAME}, ?MODULE, [Config]).

init(Config) ->
    ChildSpecs =
        [{cli_terminal_listen, {cli_terminal_listen_endpoint, start, [Config]}, transient, brutal_kill, worker, [cli_terminal_listen_endpoint]}],
    {ok, {{one_for_one, 1, 60}, ChildSpecs}}.

%% ====================================================================
%% Internal functions
%% ====================================================================