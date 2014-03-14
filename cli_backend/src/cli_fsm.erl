%% @author stdstring

-module(cli_fsm).

-behaviour(cli_fsm_behaviour).
-behaviour(gen_fsm).

-include("cli_fsm_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1, process_command/2, get_current_state/1]).
%% gen_fsm
-export([processing/3]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

start(Config) ->
    DataSource = parse_config(Config),
    SourceData = erlang_term_utils:read_from_file(DataSource),
    start_fsm(SourceData).

process_command(FsmPid, CommandName) ->
    gen_fsm:sync_send_event(FsmPid, {command, CommandName}).

get_current_state(FsmPid) ->
    gen_fsm:sync_send_event(FsmPid, current_state).

init(SourceData) ->
    StateData = create_state(SourceData),
    {ok, processing, StateData}.

processing(current_state, _From, StateData) ->
    CurrentState = StateData#cli_fsm_state.current_state,
    FsmConfig = StateData#cli_fsm_state.config,
    States = FsmConfig#fsm_config.states,
    {CurrentState, Commands} = lists:keyfind(CurrentState, 1, States),
    {reply, {CurrentState, Commands}, processing, StateData};
processing({command, CommandName}, _From, StateData) ->
    CurrentState = StateData#cli_fsm_state.current_state,
    FsmConfig = StateData#cli_fsm_state.config,
    Transitions = FsmConfig#fsm_config.transitions,
    case lists:keyfind({CurrentState, CommandName}, 1, Transitions) of
        {{CurrentState, CommandName}, NewState} ->
            NewStateData = StateData#cli_fsm_state{current_state = NewState},
            {reply, NewState, processing, NewStateData};
        false -> {reply, CurrentState, processing, StateData}
    end.

handle_event(_Event, _StateName, StateData) -> {stop, not_supported, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) -> {stop, not_supported, StateData}.

handle_info(_Event, _StateName, StateData) -> {stop, not_supported, StateData}.

terminate(_Reason, _StateName, _StateData) -> true.

code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.

%% ====================================================================
%% Internal functions
%% ====================================================================

parse_config(Config) ->
    {?CONFIG_KEY, ServiceConfig} = config_utils:get_config(Config, ?CONFIG_KEY, 1, {cli_fsm, bad_config}),
    {?DATA_SOURCE, DataSource} = config_utils:get_config(ServiceConfig, ?DATA_SOURCE, 1, {cli_fsm, missing_source}),
    DataSource.

start_fsm(SourceData) ->
    case gen_fsm:start_link(?MODULE, SourceData, []) of
        {ok, Pid} -> Pid;
        {error, Error} -> error({cli_fsm, Error})
    end.

create_state(SourceData) ->
    {initial_state, InitialState} = lists:keyfind(initial_state, 1, SourceData),
    {states, States} = lists:keyfind(states, 1, SourceData),
    {transitions, Transitions} = lists:keyfind(transitions, 1, SourceData),
    TransitionTable = lists:map(fun({FromState, ToState, CommandName}) -> {{FromState, CommandName}, ToState} end, Transitions),
    FsmConfig = #fsm_config{states = States, transitions = TransitionTable},
    #cli_fsm_state{config = FsmConfig, current_state = InitialState}.
