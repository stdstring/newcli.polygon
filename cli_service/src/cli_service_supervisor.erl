%% @author std-string

-module(cli_service_supervisor).

-behaviour(supervisor).

-include("authentication_defs.hrl").
-include("common_defs.hrl").

-export([start/1]).
%% supervisor behaviour
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(MainConfigFile :: string()) -> {'ok', Pid :: pid()} | {'error', Reason :: term()} | term().
start(MainConfigFile) ->
    GlobalConfig = config_reader:read(MainConfigFile),
    MainConfigDir = filename:dirname(MainConfigFile),
    case supervisor:start_link(?MODULE, [GlobalConfig, MainConfigDir]) of
        {ok, Pid} -> {ok, Pid};
        ignore -> ignore;
        {error, {already_started, Pid}} -> {ok, Pid};
        {error, {shutdown, Reason}} -> {error, Reason};
        {error, Reason} -> {error, Reason}
    end.

init([GlobalConfig, MainConfigDir]) ->
    CliTerminalConfig = GlobalConfig#global_config.cli_terminal,
    SevicesConfig = GlobalConfig#global_config.other,
    ChildSpecs =
        [{authentication_service, {authentication_service, start, [SevicesConfig, MainConfigDir]}, transient, brutal_kill, worker, [authentication_service]},
         {authorization_service, {authorization_service, start, [SevicesConfig, MainConfigDir]}, transient, brutal_kill, worker, [authorization_service]},
         {cli_terminal_listen_supervisor, {cli_terminal_listen_supervisor, start, [CliTerminalConfig]}, transient, brutal_kill, supervisor, [cli_terminal_listen_supervisor]},
         {cli_terminal_supervisor, {cli_terminal_supervisor, start, [GlobalConfig]}, transient, brutal_kill, supervisor, [cli_terminal_supervisor]}],
    {ok,{{one_for_one, 1, 60}, ChildSpecs}}.

%% ====================================================================
%% Internal functions
%% ====================================================================