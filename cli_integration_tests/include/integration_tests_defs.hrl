%% definitions for integration tests

-define(INPUT_DATA, "/tmp/input").

-record(integration_test_state, {backend = undefined :: 'undefined' | port(), frontend_cmd = "" :: string()}).

-define(LOGIN, "@CliDemo>login:password:").
-define(GREETING, "some greeting message").
-define(GUEST_LOGOUT, "guest@CliDemo>You are logged out.").
-define(ADMIN_LOGOUT, "root@CliDemo#You are logged out.").