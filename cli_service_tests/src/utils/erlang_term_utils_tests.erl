%% @author stdstring

-module(erlang_term_utils_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DATA_FILENAME, "/tmp/erlang_term_utils_tests.data").
-define(SOURCE_TERM, {666, iddqd, ["11", "22"]}).
-define(SOURCE_DATA, "{666,iddqd,[\"11\",\"22\"]}.\n").
-define(ADDITIONAL_DATA, "{1,iddqd,\"idkfa\"}.\n").

%% ====================================================================
%% Test functions
%% ====================================================================

read_from_file_test_() ->
    Tests = [{"read term from file", ?_assertEqual(?SOURCE_TERM, read_from_file_test_impl(?SOURCE_DATA))},
             {"try read term without trailing dot from file", ?_assertError({file_read_error, _}, read_from_file_test_impl("{a,1}"))},
             {"try read arbitrary text from file", ?_assertError({file_read_error, _}, read_from_file_test_impl("first <<text>> [portion]. 'second'\"text\" ++ portion"))},
             {"try read from file with several terms", ?_assertError({file_read_error, _}, read_from_file_test_impl("{a,1}.[11,12]."))},
             {"read from file with several terms each on own line", ?_assertEqual({a, 1}, read_from_file_test_impl("{a,1}.\n[11,12]."))},
             {"try read from unexisting file", ?_assertError({file_read_error, enoent}, erlang_term_utils:read_from_file("/tmp/some_unexisting_file.data"))}],
    {foreach, fun() -> cleanup() end, fun(_Args) -> ok end, Tests}.

write_to_file_test_() ->
    Tests = [{"write term to unexisting file", ?_assertEqual(?SOURCE_DATA, write_to_file_test_impl(?SOURCE_TERM))},
             {"write term to existing file", ?_assertEqual(?SOURCE_DATA, write_to_file_test_impl(?SOURCE_TERM, ?ADDITIONAL_DATA))}],
    {foreach, fun() -> cleanup() end, fun(_Args) -> ok end, Tests}.

append_to_file_test_() ->
    Tests = [{"append term to unexisting file", ?_assertEqual(?SOURCE_DATA, append_to_file_test_impl(?SOURCE_TERM))},
             {"append term to existing file", ?_assertEqual(?ADDITIONAL_DATA ++ ?SOURCE_DATA, append_to_file_test_impl(?SOURCE_TERM, ?ADDITIONAL_DATA))}],
    {foreach, fun() -> cleanup() end, fun(_Args) -> ok end, Tests}.

cleanup() ->
    case file:delete(?DATA_FILENAME) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, Reason} -> error({ecleanup, Reason})
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

read_from_file_test_impl(Data) ->
    WriteResult = file:write_file(?DATA_FILENAME, Data),
    ?assertEqual(ok, WriteResult),
    erlang_term_utils:read_from_file(?DATA_FILENAME).

write_to_file_test_impl(Term) ->
    WriteResult = erlang_term_utils:write_to_file(?DATA_FILENAME, Term),
    ?assertEqual(ok, WriteResult),
    {ok, BinaryData} = file:read_file(?DATA_FILENAME),
    binary_to_list(BinaryData).

write_to_file_test_impl(Term, AdditionalData) ->
    WriteResult = file:write_file(?DATA_FILENAME, AdditionalData),
    ?assertEqual(ok, WriteResult),
    write_to_file_test_impl(Term).

append_to_file_test_impl(Term) ->
    WriteResult = erlang_term_utils:append_to_file(?DATA_FILENAME, Term),
    ?assertEqual(ok, WriteResult),
    {ok, BinaryData} = file:read_file(?DATA_FILENAME),
    binary_to_list(BinaryData).

append_to_file_test_impl(Term, AdditionalData) ->
    WriteResult = file:write_file(?DATA_FILENAME, AdditionalData),
    ?assertEqual(ok, WriteResult),
    append_to_file_test_impl(Term).