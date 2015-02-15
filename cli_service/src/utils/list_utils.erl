%% @author stdstring

-module(list_utils).

-export([get_value_by_key/4, get_value_by_key_with_default/4]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_value_by_key(Source :: [tuple()], Key :: term(), KeyPosition :: integer(), KeyMissingError :: term()) -> term() | tuple().
get_value_by_key(Source, Key, KeyPosition, KeyMissingError) ->
    case lists:keyfind(Key, KeyPosition, Source) of
        false -> error(KeyMissingError);
        Result -> get_value_without_key(Result, KeyPosition)
    end.

-spec get_value_by_key_with_default(Source :: [tuple()], Key :: term(), KeyPosition :: integer(), DefaultValue :: term() | tuple()) -> term() | tuple().
get_value_by_key_with_default(Source, Key, KeyPosition, DefaultValue) ->
    case lists:keyfind(Key, KeyPosition, Source) of
        false -> DefaultValue;
        Result -> get_value_without_key(Result, KeyPosition)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec get_value_without_key(Result :: tuple(), KeyPosition :: integer()) -> term() | tuple().
get_value_without_key(Result, KeyPosition) ->
    Value = erlang:delete_element(KeyPosition, Result),
    case Value of
        {InnerValue} -> InnerValue;
        Value -> Value
    end.