%% @author std-string

-module(code_generator).

-export([generate/3]).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec generate(ModuleName :: atom(), FunctionName :: atom(), {CommandModule :: atom(), CommandFunction :: atom(), CommandArgs :: [#frame_item{}]}) ->
    {'ok', ModuleName :: atom(), ModuleBinary :: binary()} | 'false'.
generate(ModuleName, FunctionName, {CommandModule, CommandFunction, CommandArgs}) ->
    CommandExec = generate_command_execution(CommandModule, CommandFunction, CommandArgs),
    Body = [CommandExec],
    Clause = {clause, 0, [], [], Body},
    ModuleForm = {attribute, 0, module, ModuleName},
    ExportForm = {attribute, 0, export, [{FunctionName, 0}]},
    FunForm = {function, 0, FunctionName, 0, [Clause]},
    case compile:forms([ModuleForm, ExportForm, FunForm]) of
        {ok, ModuleName, ModuleBinary} -> {ok, ModuleName, ModuleBinary};
        _Other -> false
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec generate_command_execution(ModuleName :: atom(), FunctionName :: atom(), Args :: [#frame_item{}]) -> tuple().
generate_command_execution(ModuleName, FunctionName, Args) ->
    ArgsList = generate_arg_list(Args),
    io:format(user, "ArgsList: ~p~n", [ArgsList]),
    {call, 0, {remote, 0, {atom, 0, ModuleName}, {atom, 0, FunctionName}}, [ArgsList]}.


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