%% @author std-string

-module(auth_service).

-behaviour(auth_service_behaviour).
-behaviour(gen_server).

-include("common_defs.hrl").
-include("auth_service_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1, authenticate/2]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start(Config) ->
    {Filename} = parse_config(Config),
    start_service(Filename).

authenticate(Username, Password) ->
    gen_server:call(?SERVICE_NAME, {Username, Password}).

init(Filename) ->
    %% {Uid, Username, Password, AccessLevel}
    AuthData = load_auth_data(Filename),
    register(?SERVICE_NAME, self()),
    {ok, AuthData}.

handle_call({Username, Password}, _From, AuthState) ->
    AuthData = AuthState#auth_service_state.auth_data,
    case lists:keyfind(Username, 2, AuthData) of
        {Uid, Username, Hash, AccessLevel} ->
            ProcessResult = process_auth_data({Uid, Username, Hash, AccessLevel}, Password),
            {reply, ProcessResult, AuthState};
        false -> {reply, {auth_fail, unknown_username}, AuthState}
    end.

handle_cast(_Request, State) -> {stop, not_supported, State}.

handle_info(_Info, State) -> {stop, not_supported, State}.

terminate(_Reason, _State) -> true.

code_change(_OldVsn, _State, _Extra) -> {error, not_supported}.

%% ====================================================================
%% Internal functions
%% ====================================================================

parse_config(Config) ->
    case lists:keyfind(?AUTH_DATA_SOURCE, 1, Config) of
        {?AUTH_DATA_SOURCE, Filename} -> {Filename};
        false -> error({auth_service, bad_init_args})
    end.

start_service(Filename) ->
    case gen_server:start_link(?MODULE, Filename, []) of
        {ok, Pid} -> Pid;
        {error, Error} -> error({auth_service, Error})
    end.

load_auth_data(Filename) ->
    AuthData = erlang_term_utils:read_from_file(Filename),
    #auth_service_state{source = Filename, auth_data = AuthData}.

process_auth_data({Uid, Username, Hash, AccessLevel}, Password) ->
    PasswordHash = crypto_utils:hash(?HASH_TYPE, Password, ?HASH_SALT),
    if
        Hash == PasswordHash -> {auth_complete, #user{uid = Uid, username = Username, access_level = AccessLevel}};
        true -> {auth_fail, bad_password}
    end.
