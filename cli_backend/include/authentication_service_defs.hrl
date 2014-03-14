%% authentication_service definitions:

-define(CONFIG_KEY, authentication_service).
-define(DATA_SOURCE, data_source).
-define(HASH_SALT, "polygon-cli").
-define(HASH_TYPE, sha512).
-define(SERVICE_NAME, authentication_service).

-record(authentication_service_state, {source = "" :: string(), data = [] :: [{Uid :: integer(), Username :: string(), Password :: binary(), AccessLevel :: integer()}]}).
