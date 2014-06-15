%% definitions for integration tests

-define(INPUT_DATA, "/tmp/input").

-record(integration_test_state, {backend = undefined :: 'undefined' | port(), frontend_cmd = "" :: string()}).

-define(LOGIN, "@CliDemo>login:password:").
-define(GREETING, "some greeting message").
-define(GUEST_LOGOUT, "guest@CliDemo>You are logged out.").
-define(ADMIN_LOGOUT, "root@CliDemo#You are logged out.").
-define(COMMAND_FAIL, "Command execution failed. Return code is 255").
-define(PING_BAD_ARGS, "Command's execution is failed due to the following: {parser_fail,[112,105,110,103],{ping,creation_error,bad_args}}").
-define(ACCESS_DENIED, "Command's execution is failed due to the following: {precondition_check_fail,access_denied}").
-define(UNSUITABLE_COMMAND, "Command's execution is failed due to the following: {precondition_check_fail,unsuitable_command}").