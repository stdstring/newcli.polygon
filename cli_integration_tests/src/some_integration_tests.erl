-module(some_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MAX_LINE_LENGTH, 1000).

example_test() ->
    io:format(user, "~nexample start:~n", []),
    {ok, CurrentDir} = file:get_cwd(),
    io:format(user, "~nCurrentDir = ~p~n", [CurrentDir]),
    ok.