%% @author std-string

-module(authentication_service).

-behaviour(authentication_service_behaviour).
-behaviour(gen_server).

-include("common_defs.hrl").
-include("authentication_service_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/2, authenticate/2]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-spec start(Config :: [{Key :: atom(), Value :: term()}], MainConfigDir :: string()) -> pid() | {'error', Error :: term()} | no_return().
start(Config, MainConfigDir) ->
    Filename = parse_config(Config),
    start_service(filename:absname(Filename, MainConfigDir)).

-spec authenticate(Username :: string(), Password :: string()) -> {'authentication_complete', #user{}} | {'authentication_fail', Reason :: atom()}.
%% todo (std_string) : use PasswordHash instead of Password here
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
        false -> {reply, {authentication_fail, unknown_username}, State}
    end.

handle_cast(_Request, State) -> {stop, not_supported, State}.

handle_info(_Info, State) -> {stop, not_supported, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

parse_config(Config) ->
    ServiceConfig = config_utils:get_config(Config, ?CONFIG_KEY, 1, {authentication_service, bad_config}),
    config_utils:get_config(ServiceConfig, ?DATA_SOURCE, 1, {authentication_service, missing_source}).

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
        Hash == PasswordHash -> {authentication_complete, #user{uid = Uid, username = Username, access_level = AccessLevel}};
        true -> {authentication_fail, bad_password}
    end.
