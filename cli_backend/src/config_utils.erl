%% @author stdstring

-module(config_utils).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_config/4]).

%% config is the following [{Key, Value1, Value2, ...}, ...]
get_config(Config, Key, KeyPosition, KeyMissingError) ->
    case lists:keyfind(Key, KeyPosition, Config) of
        false -> KeyMissingError;
        Item -> Item
    end.