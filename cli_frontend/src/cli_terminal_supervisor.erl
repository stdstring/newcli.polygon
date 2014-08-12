%% @author std-string

-module(cli_terminal_supervisor).

-behaviour(supervisor).

-include("cli_terminal_common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/0, create_endpoint/1]).
%% supervisor behaviour
-export([init/1]).

start() ->
    supervisor:start_link({local, ?ENDPOINT_SUPERVISOR_NAME}, ?MODULE, []).

create_endpoint(TerminalState) ->
    supervisor:start_child(?ENDPOINT_SUPERVISOR_NAME, [TerminalState]).

init(_Args) ->
    ChildSpecs =
        [{cli_terminal_endpoint, {cli_terminal_endpoint, start, []}, temporary, brutal_kill, worker, [cli_terminal_endpoint]}],
    {ok, {{one_for_one, 1, 60}, ChildSpecs}}.

%% ====================================================================
%% Internal functions
%% ====================================================================