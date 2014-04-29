%% @author stdstring

-module(autocomplete_factory).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([create_expand_fun/1]).

-spec create_expand_fun(ExecutionState :: #execution_state{}) -> fun((string()) -> {'yes' | 'no', string(), [string()]}).
create_expand_fun(ExecutionState) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

%%-spec filter_command(CommandBody :: [string()], Prefix :: [string()]) -> boolean().
%%filter_command([], _PrefixRest) -> false;
%%filter_command([CommandBodyPart | _CommandBodyRest], [PartialPrefixPart]) ->
%%    lists:prefix(PartialPrefixPart, CommandBodyPart);
%%filter_command([CommandBodyPart | CommandBodyRest], [PrefixPart | PrefixRest]) ->
%%    case CommandBodyPart == PrefixPart of
%%        true -> filter_command(CommandBodyRest, PrefixRest);
%%        false -> false
%%    end.

%%-spec filter_commands(Commands :: [{CommandName :: atom(), CommandBody :: [string()], CommandHelp :: string()}], CommandPrefix :: [string()]) ->
%%          [{CommandName :: atom(), CommandBody :: [string()], CommandHelp :: string()}].
%%filter_commands(Commands, CommandPrefix) ->
%%    lists:filter(fun({_Name, Body, _Help}) -> filter_command(Body, CommandPrefix) end, Commands).

-spec filter_command(CommandBody :: [string()], Prefix :: [string()]) -> {Result :: boolean(), Rest :: [string()]}.
filter_command([], _PrefixRest) -> {false, []};
filter_command([CommandBodyPart | CommandBodyRest], [PartialPrefixPart]) ->
    case check_prefix(CommandBodyPart, PartialPrefixPart) of
        {true, Rest} -> {true, [Rest] ++ CommandBodyRest};
        {false, _} -> {false, []}
    end;
filter_command([Part | CommandBodyRest], [Part | PrefixRest]) ->
    filter_command(CommandBodyRest, PrefixRest);
filter_command(_CommandBodyRest, _PrefixRest) ->
    {false, []}.

-spec check_prefix(Source :: string(), Prefix :: string()) -> {Result :: boolean(), Rest :: string()}.
check_prefix(Prefix, Prefix) -> {true, ""};
check_prefix(Source, Prefix) when length(Source) < length(Prefix) -> {false, ""};
check_prefix(Source, Prefix) ->
    case lists:split(length(Prefix), Source) of
        {Prefix, Rest} -> {true, Rest};
        _Other -> {false, ""}
    end.