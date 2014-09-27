%% @author stdstring

-module(cli_frontend_application).

-behaviour(application).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/2, stop/1]).

start(_Type, MainConfigFile) ->
    case cli_frontend_supervisor:start(MainConfigFile) of
        {ok, Pid} -> {ok, Pid};
        {error, Reason} -> {error, Reason};
        Reason -> {error, Reason}
    end.

stop(_State) -> ok.


%% ====================================================================
%% Internal functions
%% ====================================================================
