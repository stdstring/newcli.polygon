-module(terminal_work_example).

-export([main/0]).

main() ->
    login(),
    ExpandFun = fun(_) -> {yes, "doom_", ["iddqd", "idkfa", "idclip"]} end,
    OldOptions = io:getopts(),
    NewOptions = add_option(OldOptions, expand_fun, ExpandFun),
    io:setopts(NewOptions),
    main_loop().

login() ->
    OldOptions = io:getopts(),
    NewOptions = add_option(OldOptions, echo, false),
    io:get_line(">>>>Login:"),
    io:setopts(NewOptions),
    io:get_line(">>>>Password:"),
    io:format("~n", []),
    io:setopts(OldOptions).

add_option(Options, Key, Value) ->
    lists:keystore(Key, 1, Options, {Key, Value}).

main_loop() ->
    Line = io:get_line(">>>>command:"),
    [$\n | Result] = lists:reverse(Line),
    io:format("Result: ~s~n", [Result]),
    main_loop().