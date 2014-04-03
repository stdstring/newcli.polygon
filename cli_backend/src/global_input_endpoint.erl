%% @author stdstring

-module(global_input_endpoint).
-behaviour(gen_server).

-include("message_defs.hrl").
-include("common_defs.hrl").

-record(global_state, {global_config = #global_config{} :: #global_config{}}).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start(MainConfigFile) -> ok.

init([]) -> {ok, #global_state{}}.

handle_call(#login{login_name = LoginName, password = PasswordHash}, _From, State) ->
%%    case authentication_service:authenticate(LoginName, PasswordHash) of
%%        {authentication_complete, User} ->
%%            client_input_endpoint:start(GlobalConfig, User, ClientOutput);
%%        {authentication_fail, Reason} ->
%%    end
    Reply = ok,
    {reply, Reply, State};
handle_call(#commands_info{}, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

