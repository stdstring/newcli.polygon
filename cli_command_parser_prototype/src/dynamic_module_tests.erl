%% @author std-string

-module(dynamic_module_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MODULE_NAME, some_dynamic_module).
-define(FUNCTION_NAME, some_func).
-define(POSITIVE, "positive").
-define(ZERO, "zero").
-define(NEGATIVE, "negative").

%% ====================================================================
%% Test functions
%% ====================================================================

generate_erl_syntax_module_test_() ->
    ModuleBinary = generate_erl_syntax_module(),
    {module, ?MODULE_NAME} = code:load_binary(?MODULE_NAME, [], ModuleBinary),
    [{"positive case", ?_assertEqual(?POSITIVE, ?MODULE_NAME:?FUNCTION_NAME(666))},
     {"zero case", ?_assertEqual(?ZERO, ?MODULE_NAME:?FUNCTION_NAME(0))},
     {"negative case", ?_assertEqual(?NEGATIVE, ?MODULE_NAME:?FUNCTION_NAME(-13))}].

generate_abstract_format_module_test_() ->
    ModuleBinary = generate_abstract_format_module(),
    {module, ?MODULE_NAME} = code:load_binary(?MODULE_NAME, [], ModuleBinary),
    [{"positive case", ?_assertEqual(?POSITIVE, ?MODULE_NAME:?FUNCTION_NAME(666))},
     {"zero case", ?_assertEqual(?ZERO, ?MODULE_NAME:?FUNCTION_NAME(0))},
     {"negative case", ?_assertEqual(?NEGATIVE, ?MODULE_NAME:?FUNCTION_NAME(-13))}].

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec generate_erl_syntax_module() -> binary().
generate_erl_syntax_module() ->
    % fun(X) when X > 0 -> "positive";
    % fun(X) when X == 0 -> "zero";
    % fun(X) when X < 0 -> "negative".
    ModuleDef = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(?MODULE_NAME)]),
    ModuleForm = erl_syntax:revert(ModuleDef),
    ExportList = erl_syntax:list([erl_syntax:arity_qualifier(erl_syntax:atom(?FUNCTION_NAME), erl_syntax:integer(1))]),
    ExportDef = erl_syntax:attribute(erl_syntax:atom(export), [ExportList]),
    ExportForm = erl_syntax:revert(ExportDef),
    FunArg = erl_syntax:variable("A"),
    PositiveGuard = [erl_syntax:infix_expr(FunArg, erl_syntax:operator(">"), erl_syntax:integer(0))],
    PositiveBody = [erl_syntax:string(?POSITIVE)],
    PositiveClause = erl_syntax:clause([FunArg], PositiveGuard, PositiveBody),
    ZeroGuard = [erl_syntax:infix_expr(FunArg, erl_syntax:operator("=="), erl_syntax:integer(0))],
    ZeroBody = [erl_syntax:string(?ZERO)],
    ZeroClause = erl_syntax:clause([FunArg], ZeroGuard, ZeroBody),
    NegativeGuard = [erl_syntax:infix_expr(FunArg, erl_syntax:operator("<"), erl_syntax:integer(0))],
    NegativeBody = [erl_syntax:string(?NEGATIVE)],
    NegativeClause = erl_syntax:clause([FunArg], NegativeGuard, NegativeBody),
    Fun = erl_syntax:function(erl_syntax:atom(?FUNCTION_NAME), [PositiveClause, ZeroClause, NegativeClause]),
    FunForm = erl_syntax:revert(Fun),
    % io:format(user, "ModuleForm: ~p~n", [ModuleForm]),
    % io:format(user, "ExportForm: ~p~n", [ExportForm]),
    % io:format(user, "FunForm: ~p~n", [FunForm]),
    {ok, ?MODULE_NAME, ModuleBinary} = compile:forms([ModuleForm, ExportForm, FunForm]),
    ModuleBinary.

-spec generate_abstract_format_module() -> binary().
generate_abstract_format_module() ->
    % fun(X) when X > 0 -> "positive";
    % fun(X) when X == 0 -> "zero";
    % fun(X) when X < 0 -> "negative".
    ModuleForm = {attribute, 0, module, ?MODULE_NAME},
    ExportForm = {attribute, 0, export, [{?FUNCTION_NAME, 1}]},
    PositiveGuard = {op, 0, '>', {var, 0, 'A'}, {integer, 0, 0}},
    PositiveClause = {clause, 0, [{var, 0, 'A'}], [[PositiveGuard]], [{string, 0, ?POSITIVE}]},
    ZeroGuard = {op, 0, '==', {var, 0, 'A'}, {integer, 0, 0}},
    ZeroClause = {clause, 0, [{var, 0, 'A'}], [[ZeroGuard]], [{string, 0, ?ZERO}]},
    NegativeGuard = {op, 0, '<', {var, 0, 'A'}, {integer, 0, 0}},
    NegativeClause = {clause, 0, [{var, 0, 'A'}], [[NegativeGuard]], [{string, 0, ?NEGATIVE}]},
    FunForm = {function, 0, ?FUNCTION_NAME, 1, [PositiveClause, ZeroClause, NegativeClause]},
    {ok, ?MODULE_NAME, ModuleBinary} = compile:forms([ModuleForm, ExportForm, FunForm]),
    ModuleBinary.