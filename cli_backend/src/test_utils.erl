%% @author std-string
%% @todo this is the temporary solution

-module(test_utils).

%% ====================================================================
%% API functions
%% ====================================================================

-export([show_messages/0]).

show_messages() ->
    receive
        Message ->
            io:format("Message: ~p~n", [Message]),
            show_messages()
    after 0 -> ok
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

