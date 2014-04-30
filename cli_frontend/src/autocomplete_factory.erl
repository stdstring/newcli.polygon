%% @author stdstring

-module(autocomplete_factory).

-include("logic_utils_defs.hrl").
-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([create_expand_fun/1]).

-spec create_expand_fun(ExecutionState :: #execution_state{}) -> fun((string()) -> {'yes' | 'no', string(), [string()]}).
create_expand_fun(ExecutionState) ->
    CommandsInfo = ExecutionState#execution_state.commands_info,
    AccFun = fun({_Name, Body, _Help}, Storage) -> process_command(join_command_body(Body), Storage) end,
    ResultStorage = lists:foldl(AccFun, dict:new(), CommandsInfo),
    fun(Input) ->
            PreparedInput = extra_spaces_cleanup(Input),
            case dict:is_key(PreparedInput, ResultStorage) of
                true ->
                    {ok, {CommonPart, CaseList}} = dict:find(PreparedInput, ResultStorage),
                    {yes, CommonPart, CaseList};
                false -> {yes, "", []}
            end
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_command(Body :: string(), Storage :: dict()) -> dict().
process_command(Body, Storage) ->
    process_command(Body, "", Storage).

-spec process_command(BodyRest :: string(), Key :: string(), Storage :: dict()) -> dict().
process_command("", _Key, Storage) -> Storage;
process_command([Header | BodyRest], Key, Storage) ->
    NewKey = [Header] ++ Key,
    process_command_body(BodyRest, NewKey, Storage).

-spec process_command_body(BodyRest :: string(), Key :: string(), Storage :: dict()) -> dict().
process_command_body(BodyRest, Key, Storage) ->
    InitialValue = {BodyRest, []},
    UpdateFun = fun({OldCommonPart, OldCaseList}) ->
                        {CommonPart, OldRest, NewRest} = calculate_common_part(OldCommonPart, BodyRest),
                        NewCaseList = lists:map(fun(OldCase) -> OldRest ++ OldCase end, OldCaseList),
                        {CommonPart, [NewRest] ++ NewCaseList}
                end,
    dict:update(Key, UpdateFun, InitialValue, Storage).

-spec calculate_common_part(Str1 :: string(), Str2 :: string()) -> {CommonPart :: string, Rest1 :: string(), Rest2 :: string()}.
calculate_common_part(Str1, Str2) -> calculate_common_part(Str1, Str2, "").

-spec calculate_common_part(Rest1 :: string(), Rest2 :: string(), CommonPart :: string()) ->
          {CommonPart :: string, Rest1 :: string(), Rest2 :: string()}.
calculate_common_part("", "", CommonPart) ->
    {lists:reverse(CommonPart), "", ""};
calculate_common_part(Rest1, "", CommonPart) ->
    {lists:reverse(CommonPart), Rest1, ""};
calculate_common_part("", Rest2, CommonPart) ->
    {lists:reverse(CommonPart), "", Rest2};
calculate_common_part([Char | Rest1], [Char | Rest2], CommonPart) ->
    calculate_common_part(Rest1, Rest2, [Char] ++ CommonPart);
calculate_common_part(Rest1, Rest2, CommonPart) ->
    {lists:reverse(CommonPart), Rest1, Rest2}.

-spec join_command_body(CommandBodyParts :: [string()]) -> string().
join_command_body(CommandBodyParts) ->
    %% separator == space
    string:join(CommandBodyParts, " ").

-spec extra_spaces_cleanup(Source:: string()) -> string().
extra_spaces_cleanup(Source) ->
    lists:reverse(extra_spaces_cleanup(Source, false, "")).

-spec extra_spaces_cleanup(Source :: string(), IsPrevCharSpace :: boolean(), Result :: string()) -> string().
extra_spaces_cleanup("", _, Result) -> "" ++ Result;
extra_spaces_cleanup([$\s | Rest], false, Result) ->
    extra_spaces_cleanup(Rest, true, [$\s] ++ Result);
extra_spaces_cleanup([$\t | Rest], false, Result) ->
    extra_spaces_cleanup(Rest, true, [$\s] ++ Result);
extra_spaces_cleanup([$\s | Rest], true, Result) ->
    extra_spaces_cleanup(Rest, true, Result);
extra_spaces_cleanup([$\t | Rest], true, Result) ->
    extra_spaces_cleanup(Rest, true, Result);
extra_spaces_cleanup([Char | Rest], _, Result) ->
    extra_spaces_cleanup(Rest, false, [Char] ++ Result).
