%% @author std-string

-module(char_category).

-export([is_digit/1, is_letter/1, is_space/1]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec is_digit(Char :: char()) -> boolean().
is_digit(Char) ->
    lists:member(Char, "0123456789").

-spec is_letter(Char :: char()) -> boolean().
is_letter(Char) ->
    lists:member(Char, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz").

-spec is_space(Char :: char()) -> boolean().
is_space(Char) ->
    lists:member(Char, " \t\r\n").

%% ====================================================================
%% Internal functions
%% ====================================================================