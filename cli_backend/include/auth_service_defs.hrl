%% auth_service definitions:

-define(AUTH_DATA_SOURCE, auth_data_filename).
-define(HASH_SALT, "polygon-cli").
-define(HASH_TYPE, sha512).
-define(SERVICE_NAME, auth_service).

-record(auth_service_state, {source = "" :: string(), auth_data = [] :: [{Uid :: integer(), Username :: string(), Password :: binary(), AccessLevel :: integer()}]}).
