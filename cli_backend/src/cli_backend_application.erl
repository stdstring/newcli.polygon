%% @author stdstring

-module(cli_backend_application).

-behaviour(application).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/2, stop/1]).

start(_Type, MainConfigFile) ->
    case cli_backend_supervisor:start_link(MainConfigFile) of
        {ok, Pid} -> {ok, Pid};
        {error, Reason} -> {error, Reason};
        Reason -> {error, Reason}
    end.

stop(_State) -> ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

