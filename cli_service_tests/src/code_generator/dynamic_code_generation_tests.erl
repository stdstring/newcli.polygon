%% @author stdstring

-module(dynamic_code_generation_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MODULE_NAME, dynamic_code_module).
-define(FUNCTION_NAME, process).
-define(POSITIVE, "Positive").
-define(ZERO, "Zero").
-define(NEGATIVE, "Negative").

%% ====================================================================
%% Test functions
%% ====================================================================

generate_test_() ->
    [{"generate code using erl_syntax module", ?_assertEqual(?POSITIVE, test_common_body(generate_using_erl_syntax(), 667))},
     {"generate code using abstract format", ?_assertEqual(?NEGATIVE, test_common_body(generate_using_abstract_format(), -15))},
     {"generate case expression", ?_assertEqual("Four", test_common_body(generate_case(), 4))},
     {"generate if expression", ?_assertEqual(?ZERO, test_common_body(generate_if(), 0))},
     {"generate term list", ?_assertEqual(66, test_common_body(generate_term_list(), any))}].

%% ====================================================================
%% Internal functions
%% ====================================================================

test_common_body(Binary, Argument) ->
    Master = self(),
    ExecFun = fun() ->
        {module, ?MODULE_NAME} = code:load_binary(?MODULE_NAME, [], Binary),
        Result = ?MODULE_NAME:?FUNCTION_NAME(Argument),
        Master ! {result, Result}
    end,
    code:purge(?MODULE_NAME),
    spawn_link(ExecFun),
    receive
        {result, ActualResult} ->
            DeleteResult = code:delete(?MODULE_NAME),
            ?assert(DeleteResult),
            ?assertEqual(non_existing, code:which(?MODULE_NAME)),
            ActualResult
    end.

generate_using_erl_syntax() ->
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
    {ok, ?MODULE_NAME, Binary} = compile:forms([ModuleForm, ExportForm, FunForm]),
    Binary.

generate_using_abstract_format() ->
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
    {ok, ?MODULE_NAME, Binary} = compile:forms([ModuleForm, ExportForm, FunForm]),
    Binary.

generate_case() ->
    %% case A of
    %%     1 -> "One";
    %%     4 -> "Four";
    %%     _ -> "Other";
    %% end
    Expr = {var, 0, 'A'},
    Clause1 = {clause, 0, [{integer, 0, 1}], [], [{string, 0, "One"}]},
    Clause4 = {clause, 0, [{integer, 0, 4}], [], [{string, 0, "Four"}]},
    ClauseOther = {clause, 0, [{var, 0, '_'}], [], [{string, 0, "Other"}]},
    Body = [{'case', 0, Expr, [Clause1, Clause4, ClauseOther]}],
    generate_module(Body).

generate_if() ->
    %% if
    %%     A > 0 -> "Positive";
    %%     A == 0 -> "Zero";
    %%     A < 0 -> "Negative"
    %% end
    PositiveClause = {clause, 0, [], [[{op, 0, '>', {var, 0, 'A'}, {integer, 0, 0}}]], [{string, 0, ?POSITIVE}]},
    ZeroClause = {clause, 0, [], [[{op, 0, '==', {var, 0, 'A'}, {integer, 0, 0}}]], [{string, 0, ?ZERO}]},
    NegativeClause = {clause, 0, [], [[{op, 0, '<', {var, 0, 'A'}, {integer, 0, 0}}]], [{string, 0, ?NEGATIVE}]},
    Body = [{'if', 0, [PositiveClause, ZeroClause, NegativeClause]}],
    generate_module(Body).

generate_term_list() ->
    %% fun(_) -> lists:sum([11,22,33])
    List = {cons, 0, {integer, 0, 11}, {cons, 0, {integer, 0, 22}, {cons, 0, {integer, 0, 33}, {nil, 0}}}},
    Body = [{call, 0, {remote, 0, {atom, 0, lists}, {atom, 0, sum}}, [List]}],
    generate_module(Body).

generate_module(Body) ->
    FunClause = {clause, 0, [{var, 0, 'A'}], [], Body},
    ModuleForm = {attribute, 0, module, ?MODULE_NAME},
    ExportForm = {attribute, 0, export, [{?FUNCTION_NAME, 1}]},
    FunForm = {function, 0, ?FUNCTION_NAME, 1, [FunClause]},
    {ok, ?MODULE_NAME, Binary} = compile:forms([ModuleForm, ExportForm, FunForm]),
    Binary.