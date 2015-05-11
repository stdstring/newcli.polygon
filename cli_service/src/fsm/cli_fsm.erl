%% @author std-string

-module(cli_fsm).

-behaviour(cli_fsm_behaviour).
-behaviour(gen_fsm).

-include("cli_fsm_defs.hrl").
-include("config_defs.hrl").

-export([start/1, process_command/2, get_current_state/1]).
%% gen_fsm
-export([processing/3]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(SourceData :: [{Key :: atom(), Value :: term()}]) ->
    {'ok', Pid :: pid()} | {'error', Reason :: term()}.
start(SourceData) ->
    gen_fsm:start_link(?MODULE, SourceData, []).

-spec process_command(FsmPid :: pid(), CommandName :: atom()) -> #cli_fsm_state_info{}.
process_command(FsmPid, CommandName) ->
    gen_fsm:sync_send_event(FsmPid, {command, CommandName}).

-spec get_current_state(FsmPid :: pid()) -> #cli_fsm_state_info{}.
get_current_state(FsmPid) ->
    gen_fsm:sync_send_event(FsmPid, current_state).

init(SourceData) ->
    StateData = create_state(SourceData),
    {ok, processing, StateData}.

processing(current_state, _From, StateData) ->
    CurrentState = StateData#cli_fsm_state.current_state,
    FsmConfig = StateData#cli_fsm_state.config,
    StateInfo = create_state_info(CurrentState, FsmConfig),
    {reply, StateInfo, processing, StateData};
processing({command, CommandName}, _From, StateData) ->
    CurrentState = StateData#cli_fsm_state.current_state,
    FsmConfig = StateData#cli_fsm_state.config,
    Transitions = FsmConfig#cli_fsm_config.transitions,
    case lists:keyfind({CurrentState, CommandName}, 1, Transitions) of
        {{CurrentState, CommandName}, NewState} ->
            NewStateData = StateData#cli_fsm_state{current_state = NewState},
            {reply, create_state_info(NewState, FsmConfig), processing, NewStateData};
        false ->
            {reply, create_state_info(CurrentState, FsmConfig), processing, StateData}
    end.

handle_event(_Event, _StateName, StateData) -> {stop, enotsup, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) -> {stop, enotsup, not_supported, StateData}.

handle_info(_Event, StateName, StateData) -> {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) -> ok.

code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.

%% ====================================================================
%% Internal functions
%% ====================================================================

create_state(SourceData) ->
    InitialState = list_utils:get_value_by_key(SourceData, ?CLI_FSM_INIT_STATE_KEY, 1, {?MODULE, missing_initial_state}),
    States = list_utils:get_value_by_key(SourceData, ?CLI_FSM_STATES_KEY, 1, {?MODULE, missing_states}),
    TransitionSource = list_utils:get_value_by_key(SourceData, ?CLI_FSM_TRANSITIONS_KEY, 1, {?MODULE, missing_transitions}),
    TransitionTable = lists:map(fun({FromState, ToState, CommandName}) -> {{FromState, CommandName}, ToState} end, TransitionSource),
    FsmConfig = #cli_fsm_config{states = States, transitions = TransitionTable},
    #cli_fsm_state{config = FsmConfig, current_state = InitialState}.

-spec create_state_info(CurrentState :: atom(), FsmConfig :: #cli_fsm_config{}) -> #cli_fsm_state_info{}.
create_state_info(CurrentState, FsmConfig) ->
    States = FsmConfig#cli_fsm_config.states,
    {Representation, Commands, ExitCommand} = list_utils:get_value_by_key(States, CurrentState, 1, {?MODULE, unknown_state}),
    #cli_fsm_state_info{current_state = CurrentState, current_state_representation = Representation, commands = Commands, exit_command = ExitCommand}.
