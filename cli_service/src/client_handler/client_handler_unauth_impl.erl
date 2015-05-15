%% @author std-string

-module(client_handler_unauth_impl).

-behaviour(gen_server).

-include("authentication_defs.hrl").
-include("client_handler_defs.hrl").
-include("command_defs.hrl").
-include("common_defs.hrl").
-include("crypto_defs.hrl").

-define(LOGIN_COUNT_EXCEED, "Count of login attempts is exceeded\n").
-define(DEFAULT_GREETING, "Default greeting message\n").
-define(UNKNOWN_USER_MESSAGE, "Login's attempt is failed due to the following: unknown user\n").
-define(BAD_PASSWORD_MESSAGE, "Login's attempt is failed due to the following: bad password\n").
-define(LOGIN_FAILED_MESSAGE, "Login's attempt is failed\n").
-define(CLI_FSM_CREATION_ERROR_MESSAGE, "Creation error of client handler\n").

%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================

init({GlobalConfig, Endpoint, SocketInfo}) ->
    %% for catching exit signals from commands
    process_flag(trap_exit, true),
    CommandModule = module_name_generator:generate(?ENTRY_MODULE_PREFIX, SocketInfo),
    InitState = #client_handler_unauth_state{config = GlobalConfig, endpoint = Endpoint, command_module = CommandModule},
    State = client_downtime_timer:start(InitState),
    {ok, State}.

handle_call({?LOGIN, Username, Password}, From, State) ->
    StateStage1 = client_downtime_timer:stop(State),
    GlobalConfig = StateStage1#client_handler_unauth_state.config,
    TerminalConfig = GlobalConfig#global_config.cli_terminal,
    MaxLoginCount = TerminalConfig#cli_terminal_config.max_login_attempt_count,
    LoginCount = StateStage1#client_handler_unauth_state.login_attempt_count + 1,
    LoginResult = process_login(Username, Password, LoginCount, MaxLoginCount),
    case LoginResult of
        #login_success{} ->
            process_login_success(LoginResult, From, StateStage1);
        #login_fail{} ->
            StateStage2 = client_downtime_timer:start(StateStage1),
            {reply, LoginResult, StateStage2#client_handler_unauth_state{login_attempt_count = LoginCount}};
        #login_error{} ->
            {reply, LoginResult, StateStage1}
    end.

handle_cast(_Request, State) -> {stop, enotsup, State}.

%% timer
handle_info({timeout, TimerRef, downtime}, State) ->
    case State#client_handler_unauth_state.timer_ref of
        TimerRef ->
            NewState = State#client_handler_unauth_state{timer_ref = undefined},
            Endpoint = NewState#client_handler_unauth_state.endpoint,
            cli_terminal_endpoint:stop(Endpoint, downtime),
            {noreply, NewState};
        _Other -> {noreply, State}
    end;
%% other info
handle_info(_Info, State) -> {stop, enotsup, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_login(Username :: string(), Password :: string(), LoginCount :: non_neg_integer(), MaxLoginCount :: non_neg_integer()) ->
    #login_success{} | #login_fail{} | #login_error{}.
%%process_login(Username, Password, _LoginCount, 0) ->
%%    AuthResult = process_authenticate(Username, Password),
%%    case AuthResult of
%%        {authentication_complete, User} -> #login_success{user = User, greeting = ?DEFAULT_GREETING};
%%        {authentication_fail, unknown_username} -> #login_fail{reason = ?UNKNOWN_USER_MESSAGE};
%%        {authentication_fail, bad_password} -> #login_fail{reason = ?BAD_PASSWORD_MESSAGE};
%%        {authentication_fail, _Reason} -> #login_error{reason = ?LOGIN_FAILED_MESSAGE}
%%    end;
%%process_login(_Username, _Password, LoginCount, MaxLoginCount) when LoginCount > MaxLoginCount and MaxLoginCount /= 0 ->
%%    #login_error{reason = ?LOGIN_COUNT_EXCEED};
process_login(Username, Password, LoginCount, LoginCount) ->
    AuthResult = process_authenticate(Username, Password),
    case AuthResult of
        {authentication_complete, User} -> #login_success{user = User, greeting = ?DEFAULT_GREETING};
        {authentication_fail, unknown_username} -> #login_error{reason = ?UNKNOWN_USER_MESSAGE ++ ?LOGIN_COUNT_EXCEED};
        {authentication_fail, bad_password} -> #login_error{reason = ?BAD_PASSWORD_MESSAGE ++ ?LOGIN_COUNT_EXCEED};
        {authentication_fail, _Reason} -> #login_error{reason = ?LOGIN_FAILED_MESSAGE}
    end;
process_login(Username, Password, _LoginCount, _MaxLoginCount) ->
    AuthResult = process_authenticate(Username, Password),
    case AuthResult of
        {authentication_complete, User} -> #login_success{user = User, greeting = ?DEFAULT_GREETING};
        {authentication_fail, unknown_username} -> #login_fail{reason = ?UNKNOWN_USER_MESSAGE};
        {authentication_fail, bad_password} -> #login_fail{reason = ?BAD_PASSWORD_MESSAGE};
        {authentication_fail, _Reason} -> #login_fail{reason = ?LOGIN_FAILED_MESSAGE}
    end.

-spec process_authenticate(Username :: string(), Password :: string()) ->
    {'authentication_complete', #user{}} | {'authentication_fail', Reason :: atom()}.
process_authenticate(Username, Password) ->
    PasswordStr = base64:decode_to_string(Password),
    PasswordHash = crypto_utils:hash(?HASH_TYPE, PasswordStr, ?HASH_SALT),
    authentication_service:authenticate(Username, PasswordHash).

-spec process_login_success(LoginResult :: #login_success{}, From :: tuple(), State :: #client_handler_unauth_state{}) ->
    no_return() | {'reply', Reply :: #login_error{}, State :: #client_handler_unauth_state{}}.
process_login_success(#login_success{user = User} = LoginResult, From, State) ->
    GlobalConfig = State#client_handler_unauth_state.config,
    Endpoint = State#client_handler_unauth_state.endpoint,
    CommandModule = State#client_handler_unauth_state.command_module,
    case cli_fsm:start(GlobalConfig#global_config.cli_fsm) of
        {ok, CliFsm} ->
            gen_server:reply(From, LoginResult),
            StateStage1 = #client_handler_state{config = GlobalConfig,
                                                endpoint = Endpoint,
                                                command_module = CommandModule,
                                                cli_fsm =CliFsm,
                                                user = User},
            StateStage2 = client_downtime_timer:start(StateStage1),
            gen_server:enter_loop(client_handler_impl, [], StateStage2);
        {error, _Reason} ->
            Reply = #login_error{reason = ?CLI_FSM_CREATION_ERROR_MESSAGE},
            {reply, Reply, State}
    end.