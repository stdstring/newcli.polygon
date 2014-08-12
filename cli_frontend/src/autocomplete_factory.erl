%% @author stdstring

-module(autocomplete_factory).

%% ====================================================================
%% API functions
%% ====================================================================

-export([]).
%% temporary export
-export([process_command/2, process_command/4, add_case/3, join_command_body/1, extra_spaces_cleanup/1, extra_spaces_cleanup/3]).

%%-export([create_expand_fun/1]).

%%-spec create_expand_fun(ExecutionState :: #execution_state{}) -> fun((string()) -> {'yes' | 'no', string(), [string()]}).
%%create_expand_fun(ExecutionState) ->
%%    CommandsInfo = ExecutionState#execution_state.commands_info,
%%    AccFun = fun({_Name, Body, _Help}, Storage) -> process_command(join_command_body(Body), Storage) end,
%%    ResultStorage = post_process_commands(lists:foldl(AccFun, dict:new(), CommandsInfo)),
%%    fun(Input) ->
%%            PreparedInput = extra_spaces_cleanup(Input),
%%            case dict:is_key(PreparedInput, ResultStorage) of
%%                true ->
%%                    {ok, {CommonPart, CaseList}} = dict:find(PreparedInput, ResultStorage),
%%                    {yes, CommonPart, CaseList};
%%                false -> {yes, "", []}
%%            end
%%    end.

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
process_command(BodySource, [Header | BodyRest], Key, Storage) ->
    NewKey = Key ++ [Header],
    NewStorage = add_case(BodySource, NewKey, Storage),
    process_command(BodySource, BodyRest, NewKey, NewStorage).

-spec add_case(BodySource :: string(), Key :: string(), Storage :: dict()) -> dict().
add_case(BodySource, Key, Storage) ->
    InitialValue = [BodySource],
    UpdateFun = fun(OldCaseList) -> [BodySource] ++ OldCaseList end,
    dict:update(Key, UpdateFun, InitialValue, Storage).

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
