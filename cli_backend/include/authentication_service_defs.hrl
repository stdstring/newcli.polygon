%% auth_service definitions:

-define(DATA_SOURCE, data_source).
-define(HASH_SALT, "polygon-cli").
-define(HASH_TYPE, sha512).
-define(SERVICE_NAME, authentication_service).

-record(auth_service_state, {source = "" :: string(), auth_data = [] :: [{Uid :: integer(), Username :: string(), Password :: binary(), AccessLevel :: integer()}]}).
