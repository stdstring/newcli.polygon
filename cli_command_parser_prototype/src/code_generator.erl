-module(code_generator).

-export([generate/3]).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec generate(ModuleName :: atom(), FunctionName :: atom(), Args :: [#frame_item{}]) -> tuple().
generate(ModuleName, FunctionName, Args) ->
    ArgsList = generate_arg_list(Args),
    {call, 0, {remote, 0, {atom, 0, ModuleName}, {atom, 0, FunctionName}}, [ArgsList]}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec generate_arg_list(Args :: [#frame_item{}]) -> tuple().
generate_arg_list(Args) ->
    generate_arg_list(lists:reverse(Args), {nil, 0}).

-spec generate_arg_list(Args :: [#frame_item{}], GeneratedResult :: tuple()) -> tuple().
generate_arg_list([], GeneratedResult) -> GeneratedResult;
generate_arg_list([#frame_item{type = word, value = Value} | Rest], GeneratedResult) ->
    NewGeneratedResult = {cons, 0, {string, 0, Value}, GeneratedResult},
    generate_arg_list(Rest, NewGeneratedResult);
generate_arg_list([#frame_item{type = string, value = Value} | Rest], GeneratedResult) ->
    NewGeneratedResult = {cons, 0, {string, 0, Value}, GeneratedResult},
    generate_arg_list(Rest, NewGeneratedResult).