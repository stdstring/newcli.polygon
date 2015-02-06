%% @author std-string

-module(name_search_factory).

-export([create/1]).

-include("name_search_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec create(Commands :: [{CommandModule :: atom(), CommandBody :: [string()]}]) -> name_search_table().
create(Commands) ->
    InitData = lists:map(fun({Module, Body}) -> {[], Body, Module} end, Commands),
    InitGroup = group(InitData),
    process_groups(InitGroup, []).

%% ====================================================================
%% Internal functions
%% ====================================================================

group(CommandData) ->
    group(CommandData, dict:new()).

group([], Group) -> dict:to_list(Group);
group([CommandDataHead | CommandDataRest], Group) ->
    {_ProcessedData, [Word | _WordsRest], _Module} = CommandDataHead,
    [Letter | _WordRest] = Word,
    case dict:find(Letter, Group) of
        {ok, LetterGroup} ->
            NewLetterGroup = dict:append(Word, CommandDataHead, LetterGroup),
            NewGroup = dict:store(Letter, NewLetterGroup, Group),
            group(CommandDataRest, NewGroup);
        error ->
            LetterGroup = dict:append(Word, CommandDataHead, dict:new()),
            NewGroup = dict:store(Letter, LetterGroup, Group),
            group(CommandDataRest, NewGroup)
    end.

process_groups([], Dest) -> Dest;
process_groups([{_Letter, LetterGroup} | Rest], Dest) ->
    Words = dict:fetch_keys(LetterGroup),
    MinLength = calc_min_length(Words),
    NewDest = process_group(dict:to_list(LetterGroup), MinLength, Dest),
    process_groups(Rest, NewDest).

calc_min_length([_Word]) -> 1;
calc_min_length(Words) ->
    length(string_utils:get_common_prefix(Words)) + 1.

process_group([], _MinLength, Dest) -> Dest;
process_group([{_Word, [CommandData]} | Rest], MinLength, Dest) ->
    NewDest = process_command_data(CommandData, MinLength, Dest),
    process_group(Rest, MinLength, NewDest);
process_group([{_Word, CommandDataList} | Rest], MinLength, Dest) ->
    NewCommandDataList = lists:map(fun(Data) -> update_command_data(Data, MinLength) end, CommandDataList),
    {ReadyList, IncompleteList} = lists:splitwith(fun({_ProcessedData, WordsRest, _Module}) -> WordsRest == [] end, NewCommandDataList),
    NewDest = process_command_data_list(ReadyList, Dest),
    case IncompleteList of
        [] -> process_group(Rest, MinLength, Dest);
        [{ProcessedParts, Words, Module}] ->
            process_group(Rest, MinLength, process_command_data(ProcessedParts, Words, Module, NewDest));
        _Other ->
            IncompleteGroup = group(IncompleteList),
            process_group(Rest, MinLength, process_groups(IncompleteGroup, NewDest))
    end.

process_command_data_list([], Dest) -> Dest;
process_command_data_list([{ProcessedParts, Words, Module} | CommandDataRest], Dest) ->
    NewDest = process_command_data(ProcessedParts, Words, Module, Dest),
    process_command_data_list(CommandDataRest, NewDest).

process_command_data({ProcessedParts, [Word | WordsRest], Module}, MinLength, Dest) ->
    NewProcessedPart = [{Word, MinLength}] ++ ProcessedParts,
    process_command_data(NewProcessedPart, WordsRest, Module, Dest).

process_command_data(ProcessedParts, [], Module, Dest) ->
    SearchEntry = {lists:reverse(ProcessedParts), Module},
    [SearchEntry] ++ Dest;
process_command_data(ProcessedParts, [Word | WordsRest], Module, Dest) ->
    NewProcessedPart = [{Word, 1}] ++ ProcessedParts,
    process_command_data(NewProcessedPart, WordsRest, Module, Dest).

update_command_data({ProcessedParts, [Word | WordsRest], Module}, MinLength) ->
    {[{Word, MinLength}] ++ ProcessedParts, WordsRest, Module}.