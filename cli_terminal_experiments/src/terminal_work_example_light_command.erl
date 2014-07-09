-module(terminal_work_example_light_command).

-export([start/0]).

start() ->
    Master = self(),
    spawn_link(fun() -> execute(Master) end).

execute(Master) ->
    Master ! {response, command_continue, "iddqd ... idkfa ... idclip\n"},
    Master ! {response, command_continue, "impulse 666\n"},
    Master ! {response, command_finish}.