%% @author std-string

-module(authentication_service).

-behaviour(authentication_service_behaviour).
-behaviour(gen_server).

-include("authentication_defs.hrl").

-export([start/2, authenticate/2]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec start(Config :: [{Key :: atom(), Value :: term()}], MainConfigDir :: string()) -> pid() | no_return().
start(Config, MainConfigDir) ->
    Filename = parse_config(Config),
    gen_server:start_link(?MODULE, filename:absname(Filename, MainConfigDir), []).

-spec authenticate(Username :: string(), PasswordHash :: binary()) -> {'authentication_complete', #user{}} | {'authentication_fail', Reason :: atom()}.
authenticate(Username, PasswordHash) ->
    gen_server:call(?AUTHENTICATION_SERVICE, {Username, PasswordHash}).

init(Filename) ->
    %% {Uid, Username, Password, AccessLevel}
    AbsFilename = filename:absname(Filename),
    State = load_data(AbsFilename),
    register(?AUTHENTICATION_SERVICE, self()),
    {ok, State}.

handle_call({Username, PasswordHash}, _From, State) ->
    Data = State#authentication_service_state.data,
    case lists:keyfind(Username, 2, Data) of
        {Uid, Username, Hash, AccessLevel} ->
            ProcessResult = authenticate_impl({Uid, Username, Hash, AccessLevel}, PasswordHash),
            {reply, ProcessResult, State};
        false -> {reply, {authentication_fail, unknown_username}, State}
    end.

handle_cast(_Request, State) -> {stop, enotsup, State}.

handle_info(_Info, State) -> {stop, enotsup, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec parse_config(Config :: [{Key :: atom(), Value :: term()}]) -> string() | no_return().
parse_config(Config) ->
    ServiceConfig = config_utils:get_config(Config, ?AUTHENTICATION_CONFIG, 1, {authentication_service, bad_config}),
    config_utils:get_config(ServiceConfig, ?AUTHENTICATION_DATA, 1, {authentication_service, missing_source}).

-spec load_data(Filename :: string()) -> #authentication_service_state{}.
load_data(Filename) ->
    Data = erlang_term_utils:read_from_file(Filename),
    #authentication_service_state{source = Filename, data = Data}.

-spec authenticate_impl({Uid :: integer(), Username :: string(), Hash :: binary(), AccessLevel :: integer()}, Password :: binary()) ->
          {'authentication_complete', #user{}} | {'authentication_fail', Reason :: term()}.
authenticate_impl({Uid, Username, Hash, AccessLevel}, PasswordHash) ->
    if
        Hash == PasswordHash -> {authentication_complete, #user{uid = Uid, username = Username, access_level = AccessLevel}};
        true -> {authentication_fail, bad_password}
    end.
