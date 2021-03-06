%% @author std-string

-module(dynamic_module_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MODULE_NAME, some_dynamic_module).
-define(ALT_MODULE_NAME, alt_dynamic_module).
-define(FUNCTION_NAME, some_func).
-define(POSITIVE, "positive").
-define(ZERO, "zero").
-define(NEGATIVE, "negative").
-define(POSITIVE_OTHER, "other positive").
-define(ZERO_OTHER, "other zero").
-define(NEGATIVE_OTHER, "other negative").

%% ====================================================================
%% Test functions
%% ====================================================================

generate_erl_syntax_module_test_() ->
    ModuleBinary = generate_erl_syntax_module(?POSITIVE, ?ZERO, ?NEGATIVE),
    {module, ?MODULE_NAME} = code:load_binary(?MODULE_NAME, [], ModuleBinary),
    [{"positive case", ?_assertEqual(?POSITIVE, ?MODULE_NAME:?FUNCTION_NAME(666))},
     {"zero case", ?_assertEqual(?ZERO, ?MODULE_NAME:?FUNCTION_NAME(0))},
     {"negative case", ?_assertEqual(?NEGATIVE, ?MODULE_NAME:?FUNCTION_NAME(-13))}].

generate_abstract_format_module_test_() ->
    ModuleBinary = generate_abstract_format_module(?POSITIVE, ?ZERO, ?NEGATIVE),
    {module, ?MODULE_NAME} = code:load_binary(?MODULE_NAME, [], ModuleBinary),
    [{"positive case", ?_assertEqual(?POSITIVE, ?MODULE_NAME:?FUNCTION_NAME(666))},
     {"zero case", ?_assertEqual(?ZERO, ?MODULE_NAME:?FUNCTION_NAME(0))},
     {"negative case", ?_assertEqual(?NEGATIVE, ?MODULE_NAME:?FUNCTION_NAME(-13))}].

regenerate_module_test_() ->
    OldModuleBinary = generate_abstract_format_module(?POSITIVE, ?ZERO, ?NEGATIVE),
    {module, ?MODULE_NAME} = code:load_binary(?MODULE_NAME, [], OldModuleBinary),
    NewModuleBinary = generate_abstract_format_module(?POSITIVE_OTHER, ?ZERO_OTHER, ?NEGATIVE_OTHER),
    {module, ?MODULE_NAME} = code:load_binary(?MODULE_NAME, [], NewModuleBinary),
    [{"positive case", ?_assertEqual(?POSITIVE_OTHER, ?MODULE_NAME:?FUNCTION_NAME(666))},
     {"zero case", ?_assertEqual(?ZERO_OTHER, ?MODULE_NAME:?FUNCTION_NAME(0))},
     {"negative case", ?_assertEqual(?NEGATIVE_OTHER, ?MODULE_NAME:?FUNCTION_NAME(-13))}].

generate_term_list_test() ->
    %% fun() -> lists:sum([1,2,3,4])
    List = {cons, 0, {integer, 0, 11}, {cons, 0, {integer, 0, 22}, {cons, 0, {integer, 0, 33}, {nil, 0}}}},
    Body = {call, 0, {remote, 0, {atom, 0, lists}, {atom, 0, sum}}, [List]},
    Clause = {clause, 0, [], [], [Body]},
    ModuleForm = {attribute, 0, module, ?MODULE_NAME},
    ExportForm = {attribute, 0, export, [{?FUNCTION_NAME, 0}]},
    FunForm = {function, 0, ?FUNCTION_NAME, 0, [Clause]},
    {ok, ?MODULE_NAME, ModuleBinary} = compile:forms([ModuleForm, ExportForm, FunForm]),
    {module, ?MODULE_NAME} = code:load_binary(?MODULE_NAME, [], ModuleBinary),
    ?assertEqual(66, ?MODULE_NAME:?FUNCTION_NAME()).

module_unload_test() ->
    ?assertEqual(non_existing, code:which(?ALT_MODULE_NAME)),
    ModuleBinary = generate_abstract_format_module(?ALT_MODULE_NAME, ?POSITIVE, ?ZERO, ?NEGATIVE),
    Master = self(),
    RemoteFun = fun() ->
        {module, ?ALT_MODULE_NAME} = code:load_binary(?ALT_MODULE_NAME, [], ModuleBinary),
        Result = ?ALT_MODULE_NAME:?FUNCTION_NAME(333),
        Master ! {result, Result}
    end,
    spawn_link(RemoteFun),
    receive
        {result, Result} ->
            io:format(user, "Result = ~p~n", [Result]),
            DeleteResult = code:delete(?ALT_MODULE_NAME),
            ?assert(DeleteResult),
            ?assertEqual(non_existing, code:which(?ALT_MODULE_NAME))
    end,
    ok.

generate_case_test() ->
    Expr = {var, 0, 'A'},
    Clause1 = {clause, 0, [{integer, 0, 1}], [], [{string, 0, "One"}]},
    Clause2 = {clause, 0, [{integer, 0, 4}], [], [{string, 0, "Four"}]},
    CommonClause = {clause, 0, [{var, 0, '_'}], [], [{string, 0, "Other"}]},
    CaseExpr = {'case', 0, Expr, [Clause1, Clause2, CommonClause]},
    Body = [CaseExpr],
    FunClause = {clause, 0, [{var, 0, 'A'}], [], Body},
    ModuleForm = {attribute, 0, module, ?MODULE_NAME},
    ExportForm = {attribute, 0, export, [{?FUNCTION_NAME, 1}]},
    FunForm = {function, 0, ?FUNCTION_NAME, 1, [FunClause]},
    {ok, ?MODULE_NAME, ModuleBinary} = compile:forms([ModuleForm, ExportForm, FunForm]),
    {module, ?MODULE_NAME} = code:load_binary(?MODULE_NAME, [], ModuleBinary),
    ?assertEqual("Four", ?MODULE_NAME:?FUNCTION_NAME(4)),
    ?assertEqual("One", ?MODULE_NAME:?FUNCTION_NAME(1)),
    ?assertEqual("Other", ?MODULE_NAME:?FUNCTION_NAME(666)).

generate_if_test() ->
    PositiveClause = {clause, 0, [], [[{op, 0, '>', {var, 0, 'A'}, {integer, 0, 0}}]], [{string, 0, ?POSITIVE}]},
    ZeroClause = {clause, 0, [], [[{op, 0, '==', {var, 0, 'A'}, {integer, 0, 0}}]], [{string, 0, ?ZERO}]},
    NegativeClause = {clause, 0, [], [[{op, 0, '<', {var, 0, 'A'}, {integer, 0, 0}}]], [{string, 0, ?NEGATIVE}]},
    IfExpr = {'if', 0, [PositiveClause, ZeroClause, NegativeClause]},
    Body = [IfExpr],
    FunClause = {clause, 0, [{var, 0, 'A'}], [], Body},
    ModuleForm = {attribute, 0, module, ?MODULE_NAME},
    ExportForm = {attribute, 0, export, [{?FUNCTION_NAME, 1}]},
    FunForm = {function, 0, ?FUNCTION_NAME, 1, [FunClause]},
    {ok, ?MODULE_NAME, ModuleBinary} = compile:forms([ModuleForm, ExportForm, FunForm]),
    {module, ?MODULE_NAME} = code:load_binary(?MODULE_NAME, [], ModuleBinary),
    ?assertEqual(?POSITIVE, ?MODULE_NAME:?FUNCTION_NAME(666)),
    ?assertEqual(?ZERO, ?MODULE_NAME:?FUNCTION_NAME(0)),
    ?assertEqual(?NEGATIVE, ?MODULE_NAME:?FUNCTION_NAME(-17)).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec generate_erl_syntax_module(PositiveValue :: string(), ZeroValue :: string(), NegativeValue :: string()) -> binary().
generate_erl_syntax_module(PositiveValue, ZeroValue, NegativeValue) ->
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
    PositiveBody = [erl_syntax:string(PositiveValue)],
    PositiveClause = erl_syntax:clause([FunArg], PositiveGuard, PositiveBody),
    ZeroGuard = [erl_syntax:infix_expr(FunArg, erl_syntax:operator("=="), erl_syntax:integer(0))],
    ZeroBody = [erl_syntax:string(ZeroValue)],
    ZeroClause = erl_syntax:clause([FunArg], ZeroGuard, ZeroBody),
    NegativeGuard = [erl_syntax:infix_expr(FunArg, erl_syntax:operator("<"), erl_syntax:integer(0))],
    NegativeBody = [erl_syntax:string(NegativeValue)],
    NegativeClause = erl_syntax:clause([FunArg], NegativeGuard, NegativeBody),
    Fun = erl_syntax:function(erl_syntax:atom(?FUNCTION_NAME), [PositiveClause, ZeroClause, NegativeClause]),
    FunForm = erl_syntax:revert(Fun),
    {ok, ?MODULE_NAME, ModuleBinary} = compile:forms([ModuleForm, ExportForm, FunForm]),
    ModuleBinary.

-spec generate_abstract_format_module(PositiveValue :: string(), ZeroValue :: string(), NegativeValue :: string()) -> binary().
generate_abstract_format_module(PositiveValue, ZeroValue, NegativeValue) ->
    generate_abstract_format_module(?MODULE_NAME, PositiveValue, ZeroValue, NegativeValue).

-spec generate_abstract_format_module(ModuleName :: atom(), PositiveValue :: string(), ZeroValue :: string(), NegativeValue :: string()) -> binary().
generate_abstract_format_module(ModuleName, PositiveValue, ZeroValue, NegativeValue) ->
    % fun(X) when X > 0 -> "positive";
    % fun(X) when X == 0 -> "zero";
    % fun(X) when X < 0 -> "negative".
    ModuleForm = {attribute, 0, module, ModuleName},
    ExportForm = {attribute, 0, export, [{?FUNCTION_NAME, 1}]},
    PositiveGuard = {op, 0, '>', {var, 0, 'A'}, {integer, 0, 0}},
    PositiveClause = {clause, 0, [{var, 0, 'A'}], [[PositiveGuard]], [{string, 0, PositiveValue}]},
    ZeroGuard = {op, 0, '==', {var, 0, 'A'}, {integer, 0, 0}},
    ZeroClause = {clause, 0, [{var, 0, 'A'}], [[ZeroGuard]], [{string, 0, ZeroValue}]},
    NegativeGuard = {op, 0, '<', {var, 0, 'A'}, {integer, 0, 0}},
    NegativeClause = {clause, 0, [{var, 0, 'A'}], [[NegativeGuard]], [{string, 0, NegativeValue}]},
    FunForm = {function, 0, ?FUNCTION_NAME, 1, [PositiveClause, ZeroClause, NegativeClause]},
    {ok, ModuleName, ModuleBinary} = compile:forms([ModuleForm, ExportForm, FunForm]),
    ModuleBinary.