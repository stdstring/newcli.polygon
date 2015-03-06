%% @author std-string

-module(commandline_parser_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

parse_test_() ->
    [check("parse 'abc'", "abc", ["abc"]),
     check("parse 'abc xyz'", "abc xyz", ["abc", "xyz"]),
     check("parse 'abc \\t xyz'", "abc \t xyz", ["abc", "xyz"]),
     check("parse '\"abc\"'", "\"abc\"", ["abc"]),
     check("parse '\"ab\\\"c\"'", "ab\"c", ["ab\"c"]),
     check("parse '\"ab\\\\c\"'", "ab\\c", ["ab\\c"]),
     check("parse 'abc\"xyz'", "abc\"xyz", ["abc\"xyz"]),
     check("parse '\"abc\"xyz'", "\"abc\"xyz", ["abc\"xyz"]),
     check("parse 'abc '", "abc ", ["abc"])].

%% ====================================================================
%% Internal functions
%% ====================================================================

check(Description, CommandLine, Expected) ->
    {Description, ?_assertEqual(Expected, commandline_parser:parse(CommandLine))}.