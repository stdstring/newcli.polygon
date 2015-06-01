%% command definitions

-record(module_defs, {io_buffer_module = undefined :: 'undefined' | atom(),
                      client_handler_module = undefined :: 'undefined' | atom(),
                      cli_fsm_module = undefined :: 'undefined' | atom(),
                      exec_checker_module = undefined :: 'undefined' | atom()}).

-define(ENTRY_MODULE_PREFIX, command_module).
-define(ENTRY_FUNC, execute).

%% for execution state
-define(USER_KEY, user).
-define(EX_STATE_KEY, execution_state).
-define(EX_CONTINUE, ex_continue).
-define(EX_STOP, ex_stop).

%% messages
-define(LOGIN_COUNT_EXCEED_MESSAGE, "Count of login attempts is exceeded\n").
-define(LOGIN_UNKNOWN_USER_MESSAGE, "Login's attempt is failed due to the following: unknown user\n").
-define(LOGIN_BAD_PASSWORD_MESSAGE, "Login's attempt is failed due to the following: bad password\n").
-define(LOGIN_FAILED_MESSAGE, "Login's attempt is failed\n").
-define(LOGIN_ALREADY_LOGGED_MESSAGE, "User is already logged\n").
-define(DEFAULT_GREETING_MESSAGE, "Default greeting message\n").
-define(CLI_FSM_CREATION_ERROR_MESSAGE, "Creation error of client handler\n").
-define(COMMAND_ALREADY_RUN_MESSAGE, "There is running the other command, now\n").
-define(BUFFER_START_FAIL_MESSAGE, "Buffer creation fails\n").
-define(UNSUITABLE_COMMAND_MESSAGE, "Unsuitable command\n").
-define(ACCESS_DENIED_MESSAGE, "Access denied\n").
-define(BAD_CONFIG_MESSAGE, "Bad authorization config\n").
-define(BAD_ARGS_MESSAGE, "Bad arguments\n").
-define(MISSING_USER_MESSAGE, "Missing user\n").

%% templates
-define(COMMAND_NOTFOUND_TEMPLATE, "Command \"~p\" does not found\n").
-define(COMMAND_CREATION_ERROR_TEMPLATE, "Command's creation is failed due to the following reason: ~w\n").
-define(COMMAND_FAIL_TEMPLATE, "Command execution failed. Return code is ~w\n").
-define(LOGOUT_INFO_TEMPLATE, "User ~p is logged out\n").

%% return codes
-define(BAD_ARGS_RETURN_CODE, 255).
-define(MISSING_USER_RETURN_CODE, 255).