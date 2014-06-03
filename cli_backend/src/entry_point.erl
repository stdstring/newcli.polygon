-module(entry_point).

-export([start/0, stop/0]).

start() ->
    application:start(cli_backend_application).

stop() ->
    application:stop(cli_backend_application).