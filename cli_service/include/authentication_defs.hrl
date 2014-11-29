%% authentication definitions

-define(MIN_ACCESS_LEVEL, 0).
-define(MAX_ACCESS_LEVEL, 15).

-record(user, {uid = -1 :: integer(), username = "" :: string(), access_level = ?MIN_ACCESS_LEVEL :: integer()}).

-define(CONFIG_KEY, authentication_service).
-define(DATA_SOURCE, data_source).
-define(SERVICE_NAME, authentication_service).

-record(authentication_service_state, {source = "" :: string(),
                                       data = [] :: [{Uid :: integer(), Username :: string(), Password :: binary(), AccessLevel :: integer()}]}).