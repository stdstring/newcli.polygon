%% @author stdstring

-module(config_utils).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_config/3, get_config/4]).

%% config is the following [{Key, Value1, Value2, ...}, ...]

-spec get_config(Config :: [tuple()], Key :: term(), KeyPosition :: integer()) -> term() | tuple().
get_config(Config, Key, KeyPosition) ->
    Result = lists:keyfind(Key, KeyPosition, Config),
    get_config_value(Result, KeyPosition).

-spec get_config(Config :: [tuple()], Key :: term(), KeyPosition :: integer(), KeyMissingError :: term()) -> term() | tuple().
get_config(Config, Key, KeyPosition, KeyMissingError) ->
    case lists:keyfind(Key, KeyPosition, Config) of
        false -> KeyMissingError;
        Result -> get_config_value(Result, KeyPosition)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec get_config_value(Result :: tuple(), KeyPosition :: integer()) -> term() | tuple().
get_config_value(Result, KeyPosition) ->
    Value = erlang:delete_element(KeyPosition, Result),
    case Value of
        {InnerValue} -> InnerValue;
        Value -> Value
    end.