%% @author std-string

-module(client_input_supervisor).

-behaviour(supervisor).

-include("common_defs.hrl").

-define(SUPERVISOR_NAME, {local, client_input_supervisor}).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1, create_client/2]).
%% supervisor behaviour
-export([init/1]).

-spec start(GlobalConfig :: #global_config{}) -> {'ok', Pid :: pid()} | {'error', Reason :: term()} | term().
start(GlobalConfig) ->
    supervisor:start_link(?SUPERVISOR_NAME, ?MODULE, [GlobalConfig]).

create_client(User, ClientOutput) ->
    supervisor:start_child(?SUPERVISOR_NAME, [User, ClientOutput]).

init([GlobalConfig, User, ClientOutput]) ->
    ChildSpecification = {client_input_endpoint, {client_input_endpoint, start, [GlobalConfig, User, ClientOutput]}, temporary, 2000, worker, [client_input_endpoint]},
    {ok, {{simple_one_for_one, 0, 1}, [ChildSpecification]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

