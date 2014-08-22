%% @author stdstring

-module(cli_frontend_supervisor).

-behaviour(supervisor).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1]).
%% supervisor behaviour
-export([init/1]).

-spec start(MainConfigFile :: string()) -> {'ok', Pid :: pid()} | {'error', Reason :: term()} | term().
start(MainConfigFile) ->
    GlobalConfig = config_reader:read(MainConfigFile),
    case supervisor:start_link(?MODULE, GlobalConfig) of
        {ok, Pid} -> {ok, Pid};
        ignore -> ignore;
        {error, {already_started, Pid}} -> {ok, Pid};
        {error, {shutdown, Reason}} -> {error, Reason};
        {error, Reason} -> {error, Reason}
    end.

init(GlobalConfig) ->
    CliTerminalConfig = GlobalConfig#global_config.cli_terminal,
    ChildSpecs =
        [{cli_terminal_listen_supervisor, {cli_terminal_listen_supervisor, start, [CliTerminalConfig]}, transient, brutal_kill, supervisor, [cli_terminal_listen_supervisor]}],
    {ok,{{one_for_one, 1, 60}, ChildSpecs}}.

%% ====================================================================
%% Internal functions
%% ====================================================================