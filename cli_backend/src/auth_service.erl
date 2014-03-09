%% @author std-string

-module(auth_service).

-behaviour(auth_service_behaviour).
-behaviour(gen_server).

-define(AUTH_DATA_SOURCE, auth_data_filename).
-define(HASH_SALT, "polygon-cli").
-define(SERVICE_NAME, auth_service).

-include("common_defs.hrl").

-record(auth_service_state, {source = "" :: string(), auth_data = [] :: [{Uid :: integer(), Username :: string(), Password :: binary(), AccessLevel :: integer()}]}).

%% ====================================================================
%% API functions
%% ====================================================================
-export([authenticate/2]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

authenticate(Username, Password) ->
    gen_server:call(?SERVICE_NAME, {Username, Password}).

init(Args) ->
    %% {Uid, Username, Password, AccessLevel}
    AuthData = load_auth_data(Args),
    {ok, AuthData}.

handle_call({Username, Password}, _From, AuthData) ->
    case lists:keyfind(Username, 2, AuthData#auth_service_state.auth_data) of
        {Uid, Username, Hash, AccessLevel} ->
            ProcessResult = process_auth_data({Uid, Username, Hash, AccessLevel}, Password),
            {reply, ProcessResult, AuthData};
        false -> {reply, {auth_fail, unknown_username}, AuthData}
    end.

handle_cast(_Request, _State) -> error(not_implemented).

handle_info(_Info, _State) -> error(not_implemented).

terminate(_Reason, _State) -> true.

code_change(_OldVsn, _State, _Extra) -> error(not_implemented).

%% ====================================================================
%% Internal functions
%% ====================================================================

load_auth_data(Args) when is_list(Args) ->
    case lists:keyfind(?AUTH_DATA_SOURCE, 1, Args) of
        {?AUTH_DATA_SOURCE, Filename} ->
            AuthData = erlang_term_utils:read_from_file(Filename),
            #auth_service_state{source = Filename, auth_data = AuthData};
        false -> error({auth_service_error, bad_init_args})
    end.

process_auth_data({Uid, Username, Hash, AccessLevel}, Password) ->
    PasswordHash = crypto_utils:hash(sha512, Password, ?HASH_SALT),
    if
        Hash == PasswordHash -> {auth_complete, #user{uid = Uid, username = Username, access_level = AccessLevel}};
        true -> {auth_fail, bad_password}
    end.
