%% @author stdstring

-module(global_input_endpoint).
-behaviour(gen_server).

-include("global_input_endpoint_defs.hrl").
-include("message_defs.hrl").
-include("common_defs.hrl").

-record(global_state, {global_config = #global_config{} :: #global_config{}}).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-spec start(GlobalConfig :: #global_config{}) -> {'ok', Pid :: pid()} | {'error', Reason :: term()}.
start(GlobalConfig) -> gen_server:start_link(?MODULE, GlobalConfig, []).

init(GlobalConfig) ->
    register(?SERVICE_NAME, self()),
    {ok, #global_state{global_config = GlobalConfig}}.

handle_call(#login{login_name = LoginName, password = PasswordHash}, {From, _Tag}, State) ->
    case authentication_service:authenticate(LoginName, PasswordHash) of
        {authentication_complete, User} ->
            GlobalConfig = State#global_state.global_config,
            ClientOutput = From,
            case client_input_endpoint:start(GlobalConfig, User, ClientOutput) of
                {client_input_endpoint, Error} ->
                    Reply = #login_fail{reason = {session_creation_error, Error}},
                    {reply, Reply, State};
                Pid -> 
                    Reply = #login_success{session_pid = Pid, greeting = "some greeting message"},
                    {reply, Reply, State}
            end;
        {authentication_fail, Reason} ->
            Reply = #login_fail{reason = {authentication_fail, Reason}},
            {reply, Reply, State}
    end;
handle_call(#commands_info{}, _From, State) ->
    GlobalConfig = State#global_state.global_config,
    Commands = GlobalConfig#global_config.commands,
    CommandsInfo = lists:map(fun({Name, Module}) ->
                                     Body = apply(Module, get_command_body, []),
                                     Help = apply(Module, get_help, []),
                                     #command_info{command_name = Name, command_body = Body, command_help = Help}
                             end, Commands),
    Reply = #commands_info_result{info = CommandsInfo},
    {reply, Reply, State}.

handle_cast(_Request, State) -> {stop, not_supported, State}.

handle_info(_Info, State) -> {stop, not_supported, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

