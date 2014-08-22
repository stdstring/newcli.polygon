%% @author std-string

-module(cli_terminal_supervisor).

-behaviour(supervisor).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1, create_endpoint/1]).
%% supervisor behaviour
-export([init/1]).

-spec start(GlobalConfig :: #global_config{}) -> {'ok', Pid :: pid()} | {'error', Reason :: term()}.
start(GlobalConfig) ->
    supervisor:start_link({local, ?ENDPOINT_SUPERVISOR_NAME}, ?MODULE, [GlobalConfig]).

-spec create_endpoint(Socket :: term()) -> {'ok', Pid :: pid()} | {'error', Reason :: term()}.
create_endpoint(Socket) ->
    supervisor:start_child(?ENDPOINT_SUPERVISOR_NAME, [Socket]).

init(GlobalConfig) ->
    ChildSpecs =
        [{cli_terminal_endpoint, {cli_terminal_endpoint, start, [GlobalConfig]}, temporary, brutal_kill, worker, [cli_terminal_endpoint]}],
    {ok, {{one_for_one, 1, 60}, ChildSpecs}}.

%% ====================================================================
%% Internal functions
%% ====================================================================