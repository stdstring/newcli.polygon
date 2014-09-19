-module(entry_point).

-export([start/0, stop/0]).

start() ->
    application:start(cli_frontend_application).

stop() ->
    application:stop(cli_frontend_application).