%% message definitions

%% tag
-define(COMMAND_TAG, command).
-define(COMMAND_OUTPUT_TAG, command_out).
-define(COMMAND_ERROR_TAG, command_err).
-define(COMMAND_END_TAG, 'end').
-define(COMMAND_STOP_TAG, stop).
-define(COMMAND_INT_TAG, interrupt).
-define(EXIT_TAG, exit).
-define(ERROR_TAG, error).
-define(CURRENT_MODE_EXIT_TAG, current_mode_exit).
-define(CURRENT_STATE_REQUEST_TAG, current_state_request).
-define(CURRENT_STATE_RESPONSE_TAG, current_state_response).
-define(EXTENSION_REQUEST_TAG, extension_request).
-define(EXTENSION_RESPONSE_TAG, extension_response).
-define(HELP_REQUEST_TAG, help_request).
-define(HELP_RESPONSE_TAG, help_response).
-define(SUITABLE_REQUEST_TAG, suitable_commands_request).
-define(SUITABLE_RESPONSE_TAG, suitable_commands_response).
-define(LOGIN_REQUEST_TAG, login).
-define(LOGIN_SUCCESS_RESPONSE_TAG, login_success).
-define(LOGIN_FAIL_RESPONSE_TAG, login_fail).
-define(LOGIN_ERROR_RESPONSE_TAG, login_error).

%% command interaction
-define(COMMAND_START(CommandLine), {?COMMAND_TAG, CommandLine}).
-define(COMMAND_OUTPUT(Output), {?COMMAND_OUTPUT_TAG, Output}).
-define(COMMAND_ERROR(Error), {?COMMAND_ERROR_TAG, Error}).
-define(COMMAND_END(Prompt), {?COMMAND_END_TAG, Prompt}).
-define(COMMAND_STOP, {?COMMAND_STOP_TAG, ""}).
-define(COMMAND_INT, {?COMMAND_INT_TAG}).

%% exit
-define(EXIT, {?EXIT_TAG}).

%% error
-define(ERROR(Reason), {?ERROR_TAG, Reason}).

%% current mode exit
-define(CURRENT_MODE_EXIT_REQUEST, {?CURRENT_MODE_EXIT_TAG}).
-define(CURRENT_MODE_EXIT_RESPONSE(Prompt), {?CURRENT_MODE_EXIT_TAG, Prompt}).
-define(CURRENT_MODE_STOP_RESPONSE, {?EXIT_TAG, ""}).

%% current state
-define(CURRENT_STATE_REQUEST, {?CURRENT_STATE_REQUEST_TAG}).
-define(CURRENT_STATE_RESPONSE(Prompt), {?CURRENT_STATE_RESPONSE_TAG, Prompt}).

%% extensions
-define(EXTENSION_REQUEST(CommandLine), {?EXTENSION_REQUEST_TAG, CommandLine}).
-define(EXTENSION_RESPONSE(CommonPrefix, ExtensionList), {?EXTENSION_RESPONSE_TAG, CommonPrefix, ExtensionList}).

%% help
-define(HELP_REQUEST(CommandLine), {?HELP_REQUEST_TAG, CommandLine}).
-define(HELP_RESPONSE(Help), {?HELP_RESPONSE_TAG, Help}).

%% suitable commands
-define(SUITABLE_REQUEST(CommandLine), {?SUITABLE_REQUEST_TAG, CommandLine}).
-define(SUITABLE_RESPONSE(CommandsList), {?SUITABLE_RESPONSE_TAG, CommandsList}).

%% login
-define(LOGIN_REQUEST(Username, Password), {?LOGIN_REQUEST_TAG, Username, Password}).
-define(LOGIN_SUCCESS_RESPONSE(Greeting), {?LOGIN_SUCCESS_RESPONSE_TAG, Greeting}).
-define(LOGIN_FAIL_RESPONSE(Reason), {?LOGIN_FAIL_RESPONSE_TAG, Reason}).
-define(LOGIN_ERROR_RESPONSE(Reason), {?LOGIN_ERROR_RESPONSE_TAG, Reason}).

%% notifications
-define(EXIT_NOTIFICATION(Message), {?EXIT_TAG, Message}).