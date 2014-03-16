%% @author std-string

-module(authorization_service).

-behaviour(authorization_service_behaviour).
-behaviour(gen_server).

-include("common_defs.hrl").
-include("authorization_service_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1, authorize/2]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start(Config) ->
    Filename = parse_config(Config),
    start_service(Filename).


authorize(User, CommandName) when is_record(User, user) ->
    gen_server:call(?SERVICE_NAME, {User, CommandName}).

init(Filename) ->
    %% {CommandName, AccessLevel}
    AbsFilename = filename:absname(Filename),
    State = load_data(AbsFilename),
    register(?SERVICE_NAME, self()),
    {ok, State}.

handle_call({User, CommandName}, _From, State) when is_record(User, user) ->
    Data = State#authorization_service_state.data,
    case lists:keyfind(CommandName, 1, Data) of
        {CommandName, AccessLevel} -> {reply, authorize_impl(User, AccessLevel), State};
        false -> {reply, {authorization_fail, unknown_command}, State}
    end.

handle_cast(_Request, State) -> {stop, not_supported, State}.

handle_info(_Info, State) -> {stop, not_supported, State}.

terminate(_Reason, _State) -> true.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

parse_config(Config) ->
    ServiceConfig = config_utils:get_config(Config, ?CONFIG_KEY, 1, {authorization_service, bad_config}),
    config_utils:get_config(ServiceConfig, ?DATA_SOURCE, 1, {authorization_service, missing_source}).

start_service(Filename) ->
    case gen_server:start_link(?MODULE, Filename, []) of
        {ok, Pid} -> Pid;
        {error, Error} -> error({authorization_service, Error})
    end.

load_data(Filename) ->
    Data = erlang_term_utils:read_from_file(Filename),
    #authorization_service_state{source = Filename, data = Data}.

authorize_impl(#user{access_level = UserAccessLevel}, CommandAccessLevel) ->
    if
        UserAccessLevel < CommandAccessLevel -> {authorization_result, access_denied};
        true -> {authorization_result, access_allowed}
    end.