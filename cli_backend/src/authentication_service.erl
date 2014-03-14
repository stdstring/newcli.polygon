%% @author std-string

-module(authentication_service).

-behaviour(authentication_service_behaviour).
-behaviour(gen_server).

-include("common_defs.hrl").
-include("authentication_service_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1, authenticate/2]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start(Config) ->
    Filename = parse_config(Config),
    start_service(Filename).

authenticate(Username, Password) ->
    gen_server:call(?SERVICE_NAME, {Username, Password}).

init(Filename) ->
    %% {Uid, Username, Password, AccessLevel}
    AbsFilename = filename:absname(Filename),
    State = load_data(AbsFilename),
    register(?SERVICE_NAME, self()),
    {ok, State}.

handle_call({Username, Password}, _From, State) ->
    Data = State#authentication_service_state.data,
    case lists:keyfind(Username, 2, Data) of
        {Uid, Username, Hash, AccessLevel} ->
            ProcessResult = authenticate_impl({Uid, Username, Hash, AccessLevel}, Password),
            {reply, ProcessResult, State};
        false -> {reply, {auth_fail, unknown_username}, State}
    end.

handle_cast(_Request, State) -> {stop, not_supported, State}.

handle_info(_Info, State) -> {stop, not_supported, State}.

terminate(_Reason, _State) -> true.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

parse_config(Config) ->
    {?CONFIG_KEY, ServiceConfig} = config_utils:get_config(Config, ?CONFIG_KEY, 1, {authentication_service, bad_config}),
    {?DATA_SOURCE, DataSource} = config_utils:get_config(ServiceConfig, ?DATA_SOURCE, 1, {authentication_service, missing_source}),
    DataSource.

start_service(Filename) ->
    case gen_server:start_link(?MODULE, Filename, []) of
        {ok, Pid} -> Pid;
        {error, Error} -> error({authentication_service, Error})
    end.

load_data(Filename) ->
    Data = erlang_term_utils:read_from_file(Filename),
    #authentication_service_state{source = Filename, data = Data}.

authenticate_impl({Uid, Username, Hash, AccessLevel}, Password) ->
    PasswordHash = crypto_utils:hash(?HASH_TYPE, Password, ?HASH_SALT),
    if
        Hash == PasswordHash -> {auth_complete, #user{uid = Uid, username = Username, access_level = AccessLevel}};
        true -> {auth_fail, bad_password}
    end.
