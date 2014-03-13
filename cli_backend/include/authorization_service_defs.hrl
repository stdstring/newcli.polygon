%% authorization_service definitions:

-define(DATA_SOURCE, data_source).
-define(SERVICE_NAME, authorization_service).

-record(authorization_service_state, {source = "" :: string(), data = [] :: [{CommandName :: atom(), AccessLevel :: integer()}]}).
