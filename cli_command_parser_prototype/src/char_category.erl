%% @author std-string

-module(char_category).

-export([is_digit/1, is_letter/1, is_space/1]).

%% ====================================================================
%% API functions
%% ====================================================================

is_digit(Char) ->
    lists:member(Char, "0123456789").

is_letter(Char) ->
    lists:member(Char, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz").

is_space(Char) ->
    lists:member(Char, " \t\r\n").

%% ====================================================================
%% Internal functions
%% ====================================================================