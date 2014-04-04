%% @author stdstring

-module(cli_backend_supervisor).

-behaviour(supervisor).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([start_link/1]).
%% supervisor behaviour
-export([init/1]).

-spec start_link(MainConfigFile :: string()) -> {'ok', Pid :: pid()} | {'error', Reason :: term()} | term().
start_link(MainConfigFile) ->
    GlobalConfig = config_reader:read(MainConfigFile),
    case supervisor:start_link(?MODULE, GlobalConfig) of
        {ok, Pid} -> {ok, Pid};
        ignore -> ignore;
        {error, {already_started, Pid}} -> {ok, Pid};
        {error, {shutdown, Reason}} -> {error, Reason};
        {error, Reason} -> {error, Reason}
    end.

init(GlobalConfig) ->
    MainConfigDir = GlobalConfig#global_config.main_config_dir,
    SevicesConfig = GlobalConfig#global_config.other,
    ChildSpecification =
        [{authentication_service, {authentication_service, start, [SevicesConfig, MainConfigDir]}, transient, brutal_kill, worker, [authentication_service]},
         {authorization_service, {authorization_service, start, [SevicesConfig, MainConfigDir]}, transient, brutal_kill, worker, [authorization_service]},
         {global_input_endpoint, {global_input_endpoint, start, [GlobalConfig]}, transient, brutal_kill, worker, [global_input_endpoint]}],
    {ok,{{one_for_one, 1, 60}, ChildSpecification}}.

%% ====================================================================
%% Internal functions
%% ====================================================================
