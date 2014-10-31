-module(terminal_work_example_heavy_command).

-export([start/0]).

start() ->
    Master = self(),
    spawn_link(fun() -> execute(Master) end).

execute(Master) ->
    Master ! {response, command_continue, "start heavy command ... \n"},
    timer:sleep(5000),
    Master ! {response, command_continue, "first portion of data\n"},
    timer:sleep(5000),
    Master ! {response, command_continue, "second portion of data\n"},
    timer:sleep(5000),
    Master ! {response, command_continue, "third portion of data\n"},
    timer:sleep(5000),
    Master ! {response, command_continue, "fourth portion of data\n"},
    timer:sleep(10000),
    Master ! {response, command_continue, "finish heavy command ...\n"},
    Master ! {response, command_finish}.