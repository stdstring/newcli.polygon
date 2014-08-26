%% @author stdstring

-module(commands_info_helper).

-include("backend_message_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([retrieve/1]).

-spec retrieve(GlobalHandler :: term()) ->
    {'ok', [{CommandName :: atom(), CommandBody :: [string()], CommandHelp :: string()}]} | {'error', Reason :: term()}.
retrieve(GlobalHandler) ->
    io:format("commands_info_helper:retrieve/1 ~n", []),
    %% TODO (std_string) : in future, create interface layer and use it
    case gen_server:call(GlobalHandler, #commands_info{}) of
        #commands_info_result{info = CommandsInfo} ->
            io:format("commands_info_helper:retrieve/1, CommandsInfo ~p~n", [CommandsInfo]),
            Result = lists:map(fun(#command_info{command_name = Name, command_body = Body, command_help = Help}) -> {Name, Body, Help} end, CommandsInfo),
            {ok, Result};
        #commands_info_fail{reason = Reason} ->
            io:format("commands_info_helper:retrieve/1 failed ~p~n", [Reason]),
            {error, {commands_info, Reason}}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================