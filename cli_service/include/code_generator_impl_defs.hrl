%% code generator impl definitions

-define(COMMAND_EXEC_FUN, execute).
-define(PROCESS_BUFFER_FAIL_FUN, process_buffer_fail).
-define(PROCESS_COMMAND_FUN, process_command).
-define(PROCESS_SUCCESS_FUN, process_success).
-define(PROCESS_FAIL_FUN, process_fail).
-define(SEND_OUTPUT_FUN, send_output).

-define(BUFFER_VAR, {var, 0, 'Buffer'}).
-define(CLI_FSM_VAR, {var, 0, 'CliFsm'}).
-define(CLIENT_HANDLER_VAR, {var, 0, 'ClientHandler'}).
-define(CONTEXT_VAR, {var, 0, 'Context'}).
-define(MESSAGE_VAR, {var, 0, 'Message'}).
-define(RETURN_VALUE_VAR, {var, 0, 'ReturnValue'}).