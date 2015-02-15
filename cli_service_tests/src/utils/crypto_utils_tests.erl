%% @author std-string

-module(crypto_utils_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SOURCE_DATA, "iddqd idkfa").
-define(SALT, "some salt value").

%% ====================================================================
%% Test functions
%% ====================================================================

hash_test_() ->
    [{"sha512 hash", ?_assertEqual(expected_hash_impl(sha512, ?SOURCE_DATA), crypto_utils:hash(sha512, ?SOURCE_DATA, ?SALT))},
     {"sha256 hash", ?_assertEqual(expected_hash_impl(sha256, ?SOURCE_DATA), crypto_utils:hash(sha256, ?SOURCE_DATA, ?SALT))}].

%% ====================================================================
%% Internal functions
%% ====================================================================

expected_hash_impl(Type, Data) ->
    IntermediateDigest = crypto:hash(Type, Data),
    crypto:hash(Type, list_to_binary([IntermediateDigest, ?SALT])).