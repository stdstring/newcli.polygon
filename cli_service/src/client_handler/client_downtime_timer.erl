%% @author std-string

-module(client_downtime_timer).

-include("authentication_defs.hrl").
-include("common_defs.hrl").

-export([start/1, stop/1, restart/1]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(State :: #client_handler_unauth_state{} | #client_handler_state{}) ->
    #client_handler_unauth_state{} | #client_handler_state{}.
start(#client_handler_unauth_state{} = State) ->
    Downtime = get_downtime(State),
    TimerRef = erlang:start_timer(Downtime, self(), downtime),
    State#client_handler_unauth_state{timer_ref = TimerRef};
start(#client_handler_state{} = State) ->
    Downtime = get_downtime(State),
    TimerRef = erlang:start_timer(Downtime, self(), downtime),
    State#client_handler_state{timer_ref = TimerRef}.

-spec stop(State :: #client_handler_unauth_state{} | #client_handler_state{}) ->
    #client_handler_unauth_state{} | #client_handler_state{}.
stop(#client_handler_unauth_state{} = State) ->
    TimerRef = State#client_handler_unauth_state.timer_ref,
    erlang:cancel_timer(TimerRef),
    State#client_handler_unauth_state{timer_ref = undefined};
stop(#client_handler_state{} = State) ->
    TimerRef = State#client_handler_state.timer_ref,
    erlang:cancel_timer(TimerRef),
    State#client_handler_state{timer_ref = undefined}.

-spec restart(State :: #client_handler_unauth_state{} | #client_handler_state{}) ->
    #client_handler_unauth_state{} | #client_handler_state{}.
restart(State) -> start(stop(State)).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec get_downtime(Data :: #client_handler_unauth_state{} | #client_handler_state{} | #global_config{}) -> pos_integer().
get_downtime(#client_handler_unauth_state{config = GlobalConfig}) ->
    get_downtime(GlobalConfig);
get_downtime(#client_handler_state{config = GlobalConfig}) ->
    get_downtime(GlobalConfig);
get_downtime(#global_config{cli_terminal = #cli_terminal_config{downtime = Downtime}}) ->
    Downtime * 1000.