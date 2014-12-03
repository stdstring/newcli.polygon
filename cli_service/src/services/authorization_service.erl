%% @author std-string

-module(authorization_service).

-behaviour(authorization_service_behaviour).
-behaviour(gen_server).

-include("authentication_defs.hrl").

-define(AUTHORIZATION_CONFIG, authorization_service).
-define(AUTHORIZATION_DATA, data_source).
-define(AUTHORIZATION_SERVICE, authorization_service).

-record(authorization_service_state, {data = [] :: [{CommandName :: atom(), AccessLevel :: integer()}]}).


%% ====================================================================
%% API functions
%% ====================================================================

-export([start/2, authorize_command/2, authorize_commands/2]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-spec start(Config :: [{Key :: atom(), Value :: term()}], MainConfigDir  :: string()) -> pid() | no_return().
start(Config, MainConfigDir) ->
    Filename = parse_config(Config),
    gen_server:start_link(?MODULE, filename:absname(Filename, MainConfigDir), []).

-spec authorize_command(User :: #user{}, CommandName :: atom()) ->
    {'authorization_result', 'access_allowed' | 'access_denied'} | {'authorization_fail', Reason :: atom()}.
authorize_command(User, CommandName) when is_record(User, user) ->
    gen_server:call(?AUTHORIZATION_SERVICE, {User, CommandName}).

-spec authorize_commands(User :: #user{}, CommandNames :: [atom()]) -> [atom()].
authorize_commands(_User, _CommandNames) ->
    [].

init(Filename) ->
    %% {CommandName, AccessLevel}
    AbsFilename = filename:absname(Filename),
    State = load_data(AbsFilename),
    register(?AUTHORIZATION_SERVICE, self()),
    {ok, State}.

handle_call({User, CommandName}, _From, State) when is_record(User, user) ->
    Data = State#authorization_service_state.data,
    case lists:keyfind(CommandName, 1, Data) of
        {CommandName, AccessLevel} -> {reply, authorize_impl(User, AccessLevel), State};
        false -> {reply, {authorization_fail, unknown_command}, State}
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
    ServiceConfig = config_utils:get_config(Config, ?AUTHORIZATION_CONFIG, 1, {authorization_service, bad_config}),
    config_utils:get_config(ServiceConfig, ?AUTHORIZATION_DATA, 1, {authorization_service, missing_source}).

-spec load_data(Filename :: string()) -> #authorization_service_state{}.
load_data(Filename) ->
    Data = erlang_term_utils:read_from_file(Filename),
    #authorization_service_state{data = Data}.

-spec authorize_impl(#user{}, CommandAccessLevel :: integer()) -> {'authorization_result', 'access_allowed' | 'access_denied'}.
authorize_impl(#user{access_level = UserAccessLevel}, CommandAccessLevel) ->
    if
        UserAccessLevel < CommandAccessLevel -> {authorization_result, access_denied};
        true -> {authorization_result, access_allowed}
    end.