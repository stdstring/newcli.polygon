-module(entry_point).

-export([start/0, stop/0]).

start() ->
    io:format("start()~n", []),
    io:format("current dir: ~p~n", [file:get_cwd()]),
    application:start(cli_backend_application).

stop() ->
    application:stop(cli_backend_application).