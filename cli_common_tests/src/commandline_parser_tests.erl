%% @author std-string

-module(commandline_parser_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

get_first_token_with_default_delimiter_test_() ->
    [{"empty line", ?_assertEqual({[], ""}, commandline_parser:get_first_token(""))},
     {"single word", ?_assertEqual({"iddqd", ""}, commandline_parser:get_first_token("iddqd"))},
     {"several words with single delimiter", ?_assertEqual({"iddqd", "idkfa idclip"}, commandline_parser:get_first_token("iddqd idkfa idclip"))},
     {"several words with multiple delimiter", ?_assertEqual({"iddqd", "idkfa   idclip"}, commandline_parser:get_first_token("iddqd   idkfa   idclip"))}].

get_first_token_test_() ->
    [{"empty line", ?_assertEqual({[], ""}, commandline_parser:get_first_token("", $-))},
     {"single word", ?_assertEqual({"iddqd", ""}, commandline_parser:get_first_token("iddqd", $-))},
     {"several words with single delimiter", ?_assertEqual({"iddqd", "idkfa-idclip"}, commandline_parser:get_first_token("iddqd-idkfa-idclip", $-))},
     {"several words with multiple delimiter", ?_assertEqual({"iddqd", "idkfa---idclip"}, commandline_parser:get_first_token("iddqd---idkfa---idclip", $-))}].

get_tokens_with_default_delimiter_test_() ->
    [{"empty line", ?_assertEqual([], commandline_parser:get_tokens(""))},
     {"single word", ?_assertEqual(["iddqd"], commandline_parser:get_tokens("iddqd"))},
     {"several words with single delimiter", ?_assertEqual(["iddqd", "idkfa", "idclip"], commandline_parser:get_tokens("iddqd idkfa idclip"))},
     {"several words with multiple delimiter", ?_assertEqual(["iddqd", "idkfa", "idclip"], commandline_parser:get_tokens("iddqd   idkfa   idclip"))}].

get_tokens_test_() ->
    [{"empty line", ?_assertEqual([], commandline_parser:get_tokens("", "-"))},
     {"single word", ?_assertEqual(["iddqd"], commandline_parser:get_tokens("iddqd", "-"))},
     {"several words with single delimiter", ?_assertEqual(["iddqd", "idkfa", "idclip"], commandline_parser:get_tokens("iddqd-idkfa-idclip", "-"))},
     {"several words with multiple delimiter", ?_assertEqual(["iddqd", "idkfa", "idclip"], commandline_parser:get_tokens("iddqd---idkfa---idclip", "-"))}].