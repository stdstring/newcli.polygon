%% @author std-string

-module(code_generator).

-export([generate/3]).

-include("frame_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec generate(EntryModule :: atom(), EntryFunc :: atom(), Command :: #command{}) -> 'ok'.
generate(EntryModule, EntryFunc, Command) ->
    Body = [generate_before_command(), generate_command_execution(Command), generate_after_command()],
    Clause = {clause, 0, [], [], Body},
    ModuleForm = {attribute, 0, module, EntryModule},
    ExportForm = {attribute, 0, export, [{EntryFunc, 0}]},
    FunForm = {function, 0, EntryFunc, 0, [Clause]},
    case compile:forms([ModuleForm, ExportForm, FunForm]) of
        {ok, EntryModule, Binary} -> {ok, Binary};
        _Other -> {false, undefined}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

generate_before_command() -> ok.

generate_after_command() -> ok.

-spec generate_command_execution(Command :: #command{}) -> tuple().
generate_command_execution(#command{module = Module, function = Func, arguments =Args}) ->
    ArgsList = generate_arg_list(Args),
    {call, 0, {remote, 0, {atom, 0, Module}, {atom, 0, Func}}, [ArgsList]}.

-spec generate_arg_list(Args :: [#frame_item{}]) -> tuple().
generate_arg_list(Args) ->
    generate_arg_list(lists:reverse(Args), {nil, 0}).

-spec generate_arg_list(Args :: [#argument{}], Result :: tuple()) -> tuple().
generate_arg_list([], Result) -> Result;
generate_arg_list([#argument{type = word, value = Value} | Rest], Result) ->
    NewResult = {cons, 0, {string, 0, Value}, Result},
    generate_arg_list(Rest, NewResult);
generate_arg_list([#argument{type = string, value = Value} | Rest], Result) ->
    NewResult = {cons, 0, {string, 0, Value}, Result},
    generate_arg_list(Rest, NewResult).