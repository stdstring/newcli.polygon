-module(cli_backend_life_manager).

-export([start/1, stop/1, lifetime_loop/0]).

-define(PROCESS_NAME, cli_backend_life_manager).

start(Node) ->
    spawn(Node, ?MODULE, lifetime_loop, []),
    ok.

stop(Node) ->
    {?PROCESS_NAME, Node} ! shutdown,
    ok.

lifetime_loop() ->
    register(?PROCESS_NAME, self()),
    receive
        shutdown -> init:stop()
    end.
