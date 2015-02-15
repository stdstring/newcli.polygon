%% @author std-string

-module(commandline_parser).

-export([parse/1]).

-define(COMMON_STATE, common).
-define(STRING_STATE, string).

%% ====================================================================
%% API functions
%% ====================================================================

-spec parse(CommandLine :: string()) -> [string()].
parse(CommandLine) ->
    parse_impl(CommandLine, ?COMMON_STATE, [], []).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% 'aaa bbb' -> ["aaa", "bbb"]
%% 'aaa "bbb ccc"' -> ["aaa", "bbb ccc"]
%% 'aaa "bbb\"ccc"' -> ["aaa", "bbb\"ccc"]
%% 'aaa"bbb' -> ["aaa\"bbb"]
%% '"aaa"bbb' -> ["aaa\"bbb"]
-spec parse_impl(CommandLineRest :: string(), State :: atom(), Token :: string(), Storage :: [string()]) -> [string()].
parse_impl([], _State, [], Storage) ->
    lists:reverse(Storage);
parse_impl([], _State, Token, Storage) ->
    lists:reverse([lists:reverse(Token)] ++ Storage);
parse_impl([$", $\s | Rest], ?STRING_STATE, Token, Storage) ->
    parse_impl(Rest, ?COMMON_STATE, [], [lists:reverse(Token)] ++ Storage);
parse_impl([$", $\t | Rest], ?STRING_STATE, Token, Storage) ->
    parse_impl(Rest, ?COMMON_STATE, [], [lists:reverse(Token)] ++ Storage);
parse_impl([$"], ?STRING_STATE, Token, Storage) ->
    parse_impl([], ?COMMON_STATE, [], [lists:reverse(Token)] ++ Storage);
parse_impl([$\\, Char | Rest], ?STRING_STATE, Token, Storage) ->
    parse_impl(Rest, ?STRING_STATE, [Char] ++ Token, Storage);
parse_impl([Char | Rest], ?STRING_STATE, Token, Storage) ->
    parse_impl(Rest, ?STRING_STATE, [Char] ++ Token, Storage);
parse_impl([$" | Rest], ?COMMON_STATE, [], Storage) ->
    parse_impl(Rest, ?STRING_STATE, [], Storage);
parse_impl([$\s | Rest], ?COMMON_STATE, [], Storage) ->
    parse_impl(Rest, ?COMMON_STATE, [], Storage);
parse_impl([$\t | Rest], ?COMMON_STATE, [], Storage) ->
    parse_impl(Rest, ?COMMON_STATE, [], Storage);
parse_impl([$\s | Rest], ?COMMON_STATE, Token, Storage) ->
    parse_impl(Rest, ?COMMON_STATE, [], [lists:reverse(Token)] ++ Storage);
parse_impl([$\t | Rest], ?COMMON_STATE, Token, Storage) ->
    parse_impl(Rest, ?COMMON_STATE, [], [lists:reverse(Token)] ++ Storage);
parse_impl([Char | Rest], ?COMMON_STATE, Token, Storage) ->
    parse_impl(Rest, ?COMMON_STATE, [Char] ++ Token, Storage).