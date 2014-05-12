%% @author std-string

-module(crypto_utils).

%% ====================================================================
%% API functions
%% ====================================================================

-export([hash/3]).

hash(Type, Data, Salt) ->
    IntermediateDigest = crypto:hash(Type, Data),
    crypto:hash(Type, list_to_binary([IntermediateDigest, Salt])).