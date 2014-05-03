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
    ResultStorage = post_process_commands(lists:foldl(AccFun, dict:new(), CommandsInfo)),
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
    process_command(Body, Body, undefined, Storage).

-spec process_command(BodySource :: string(), BodyRest :: string(), Key :: string(), Storage :: dict()) -> dict().
process_command(_BodySource, "", _Key, Storage) -> Storage;
process_command(BodySource, BodyRest, undefined, Storage) ->
    NewKey = "",
    NewStorage = process_body(BodySource, BodyRest, NewKey, Storage),
    process_command(BodySource, BodyRest, NewKey, NewStorage);
process_command(BodySource, [Header | BodyRest], Key, Storage) ->
    NewKey = [Header] ++ Key,
    NewStorage = process_body(BodySource, BodyRest, NewKey, Storage),
    process_command(BodySource, BodyRest, NewKey, NewStorage).

-spec process_body(BodySource :: string(), BodyRest :: string(), Key :: string(), Storage :: dict()) -> dict().
process_body(BodySource, BodyRest, Key, Storage) ->
    InitialValue = {BodyRest, [BodySource]},
    UpdateFun = fun({OldCommonPart, OldCaseList}) ->
                        CommonPart = calculate_common_part(OldCommonPart, BodyRest),
                        {CommonPart, [BodySource] ++ OldCaseList}
                end,
    dict:update(Key, UpdateFun, InitialValue, Storage).

-spec post_process_commands(Storage :: dict()) -> dict().
post_process_commands(Storage) ->
    MapFun = fun
                (_Key, {CommonPart, [_SingleCase]}) -> {CommonPart, []};
                (_Key, Value) -> Value
             end,
    Predicate = fun
                   (_Key, {"", []}) -> false;
                   (_Key, _Value) -> true
                end,
    dict:filter(Predicate, dict:map(MapFun, Storage)).

-spec calculate_common_part(Str1 :: string(), Str2 :: string()) -> string().
calculate_common_part(Str1, Str2) -> calculate_common_part(Str1, Str2, "").

-spec calculate_common_part(Rest1 :: string(), Rest2 :: string(), CommonPart :: string()) -> string().
calculate_common_part("", "", CommonPart) -> lists:reverse(CommonPart);
calculate_common_part(_Rest1, "", CommonPart) -> lists:reverse(CommonPart);
calculate_common_part("", _Rest2, CommonPart) -> lists:reverse(CommonPart);
calculate_common_part([Char | Rest1], [Char | Rest2], CommonPart) ->
    calculate_common_part(Rest1, Rest2, [Char] ++ CommonPart);
calculate_common_part(_Rest1, _Rest2, CommonPart) -> lists:reverse(CommonPart).

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
