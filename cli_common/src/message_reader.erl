%% @author std-string

-module(message_reader).

%% ====================================================================
%% API functions
%% ====================================================================

-export([read_all_messages/0]).

-spec read_all_messages() -> [Message :: term()].
read_all_messages() ->
    read_all_messages_impl([]).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec read_all_messages_impl(Messages :: [Message :: term()]) -> [Message :: term()].
read_all_messages_impl(Messages) ->
	receive
        Message -> read_all_messages_impl([Message] ++ Messages)
    after 0 -> lists:reverse(Messages)
    end.