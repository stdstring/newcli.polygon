%% @author stdstring

-module(cli_fsm).

-behaviour(cli_fsm_behaviour).
-behaviour(gen_fsm).

-include("cli_fsm_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1, process_command/1, get_current_state/0]).
%% gen_fsm
-export([processing/3]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

start(_Config) -> error(not_implemented).

process_command(_CommandName) -> error(not_implemented).

get_current_state() -> error(not_implemented).

init(_Args) -> error(not_implemented).

processing(_Event, _From, _StateData) -> error(not_implemented).

handle_event(_Event, _StateName, _StateData) -> error(not_implemented).

handle_sync_event(_Event, _From, _StateName, _StateData) -> error(not_implemented).

handle_info(_Event, _StateName, _StateData) -> error(not_implemented).

terminate(_Reason, _StateName, _StateData) -> true.

code_change(_OldVsn, _StateName, _StateData, _Extra) -> error(not_implemented).

%% ====================================================================
%% Internal functions
%% ====================================================================

parse_config(Config) ->
    {?CONFIG_KEY, ServiceConfig} = config_utils:get_config(Config, ?CONFIG_KEY, 1, {cli_fsm, bad_config}),
    {?DATA_SOURCE, DataSource} = config_utils:get_config(ServiceConfig, ?DATA_SOURCE, 1, {cli_fsm, missing_source}),
    DataSource.

create_state(SourceData) ->
    {initial_state, InitialState} = lists:keyfind(initial_state, 1, SourceData),
    {states, States} = lists:keyfind(states, 1, SourceData),
    {transitions, Transitions} = lists:keyfind(transitions, 1, SourceData),
    TransitionTable = lists:map(fun({FromState, ToState, CommandName}) -> {{FromState, CommandName}, ToState} end, Transitions),
    FsmConfig = #fsm_config{states = States, transitions = TransitionTable},
    #cli_fsm_state{config = FsmConfig, current_state = InitialState}.
