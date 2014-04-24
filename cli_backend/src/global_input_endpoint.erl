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

-export([start/1, process_login/2, process_commands_info/0]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-spec start(GlobalConfig :: #global_config{}) -> {'ok', Pid :: pid()} | {'error', Reason :: term()}.
start(GlobalConfig) -> gen_server:start_link(?MODULE, GlobalConfig, []).

-spec process_login(LoginName :: string(), Password :: binary()) -> #login_success{} | #login_fail{}.
process_login(LoginName, Password) ->
    gen_server:call(?SERVICE_NAME, #login{login_name = LoginName, password = Password}).

-spec process_commands_info() -> #commands_info_result{}.
process_commands_info() ->
    gen_server:call(?SERVICE_NAME, #commands_info{}).

init(GlobalConfig) ->
    register(?SERVICE_NAME, self()),
    {ok, #global_state{global_config = GlobalConfig}}.

handle_call(#login{login_name = LoginName, password = PasswordHash}, {From, _Tag}, State) ->
    case authentication_service:authenticate(LoginName, PasswordHash) of
        {authentication_complete, User} ->
            GlobalConfig = State#global_state.global_config,
            ClientOutput = From,
            case client_input_endpoint:start(GlobalConfig, User, ClientOutput) of
                {error, Error} ->
                    Reply = #login_fail{reason = {session_creation_error, Error}},
                    {reply, Reply, State};
                {ok, Pid} ->
                    IsAdmin = User#user.access_level == ?MAX_ACCESS_LEVEL,
                    Reply = #login_success{login_name = LoginName, is_admin = IsAdmin, session_pid = Pid, greeting = "some greeting message"},
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

handle_cast(_Request, State) -> {stop, enotsup, State}.

handle_info(_Info, State) -> {stop, enotsup, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

