%% @author std-string

-module(crypto_utils).

-export([hash/3]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec hash(Type :: atom(), Data :: string(), Salt :: string()) -> binary().
hash(Type, Data, Salt) ->
    IntermediateDigest = crypto:hash(Type, Data),
    crypto:hash(Type, list_to_binary([IntermediateDigest, Salt])).

%% ====================================================================
%% Internal functions
%% ====================================================================