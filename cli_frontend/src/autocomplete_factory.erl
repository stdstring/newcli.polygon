%% @author stdstring

-module(autocomplete_factory).

%% ====================================================================
%% API functions
%% ====================================================================

-export([create_extension_generator/1]).

-spec create_extension_generator(CommandsBody :: [[string()]]) -> fun((string()) -> [string()]).
create_extension_generator(CommandsBody) ->
    AccFun = fun(Body, Storage) -> process_command(join_command_body(Body), Storage) end,
    ResultStorage = post_process_storage(lists:foldl(AccFun, dict:new(), CommandsBody)),
    fun(Input) ->
        PreparedInput = string:strip(extra_spaces_cleanup(Input)),
        case dict:find(PreparedInput, ResultStorage) of
            {ok, CaseList} -> CaseList;
            error -> []
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
    NewStorage = add_case(BodySource, NewKey, Storage),
    process_command(BodySource, BodyRest, NewKey, NewStorage);
process_command(BodySource, [$\s | BodyRest], Key, Storage) ->
    NewKey = Key ++ [$\s],
    process_command(BodySource, BodyRest, NewKey, Storage);
process_command(BodySource, [Header | BodyRest], Key, Storage) ->
    NewKey = Key ++ [Header],
    NewStorage = add_case(BodySource, NewKey, Storage),
    process_command(BodySource, BodyRest, NewKey, NewStorage).

-spec add_case(BodySource :: string(), Key :: string(), Storage :: dict()) -> dict().
add_case(BodySource, Key, Storage) ->
    InitialValue = [BodySource],
    UpdateFun = fun(OldCaseList) -> [BodySource] ++ OldCaseList end,
    dict:update(Key, UpdateFun, InitialValue, Storage).

-spec post_process_storage(Storage :: dict()) -> dict().
post_process_storage(Storage) ->
    dict:map(fun(_Key, CaseList) -> lists:reverse(CaseList) end, Storage).

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
