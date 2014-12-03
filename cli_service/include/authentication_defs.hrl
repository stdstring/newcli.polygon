%% authentication definitions

-define(MIN_ACCESS_LEVEL, 0).
-define(MAX_ACCESS_LEVEL, 15).

-record(user, {uid = -1 :: integer(), username = "" :: string(), access_level = ?MIN_ACCESS_LEVEL :: integer()}).

-define(AUTHENTICATION_CONFIG, authentication_service).
-define(AUTHENTICATION_DATA, data_source).
-define(AUTHENTICATION_SERVICE, authentication_service).

-record(authentication_service_state, {source = "" :: string(),
                                       data = [] :: [{Uid :: integer(), Username :: string(), Password :: binary(), AccessLevel :: integer()}]}).