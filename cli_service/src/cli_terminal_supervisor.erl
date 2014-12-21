%% @author std-string

-module(cli_terminal_supervisor).

-behaviour(supervisor).

-include("common_defs.hrl").

-export([start/1, create_endpoint/1]).
%% supervisor behaviour
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(GlobalConfig :: #global_config{}) -> {'ok', Pid :: pid()} | {'error', Reason :: term()}.
start(GlobalConfig) ->
    supervisor:start_link({local, ?ENDPOINT_SUPERVISOR_NAME}, ?MODULE, GlobalConfig).

-spec create_endpoint(Socket :: term()) -> {'ok', Pid :: pid()} | {'error', Reason :: term()}.
create_endpoint(Socket) ->
    supervisor:start_child(?ENDPOINT_SUPERVISOR_NAME, [Socket]).

init(GlobalConfig) ->
    ChildSpec = {cli_terminal_endpoint, {cli_terminal_endpoint, start, [GlobalConfig]}, temporary, 2000, worker, [cli_terminal_endpoint]},
    {ok, {{simple_one_for_one, 0, 1}, [ChildSpec]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================