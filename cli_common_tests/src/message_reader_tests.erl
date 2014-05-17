%% @author stdstring

-module(message_reader_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

read_all_messages_test_() ->
    Tests = [{"empty message buffer", ?_assertEqual([], check_all_messages([]))},
             {"single message", ?_assertEqual([message], check_all_messages([message]))},
             {"several messages", ?_assertEqual([message1, {message2, 666}], check_all_messages([message1, {message2, 666}]))}],
    {foreach, fun() -> lib:flush_receive() end, fun(_Arg) -> ok end, Tests}.

send_all_messages(Messages) ->
    lists:foreach(fun(Message) -> self() ! Message end, Messages).

check_all_messages(Messages) ->
    send_all_messages(Messages),
    message_reader:read_all_messages().