%% @author std-string

-module(client_input_supervisor).

-behaviour(supervisor).

-include("common_defs.hrl").

-define(SUPERVISOR_NAME, client_input_supervisor).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1, create_client/1]).
%% supervisor behaviour
-export([init/1]).

-spec start(GlobalConfig :: #global_config{}) -> {'ok', Pid :: pid()} | {'error', Reason :: term()} | term().
start(GlobalConfig) ->
    supervisor:start_link({local, ?SUPERVISOR_NAME}, ?MODULE, GlobalConfig).

create_client(User) ->
    supervisor:start_child(?SUPERVISOR_NAME, [User]).

init(GlobalConfig) ->
    ChildSpecification = {client_input_endpoint, {client_input_endpoint, start, [GlobalConfig]}, temporary, 2000, worker, [client_input_endpoint]},
    {ok, {{simple_one_for_one, 0, 1}, [ChildSpecification]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

