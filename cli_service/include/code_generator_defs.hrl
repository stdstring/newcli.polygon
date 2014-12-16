%% code generator definitions

-record(module_defs, {io_buffer_module = undefined :: 'undefined' | atom(),
                      client_handler_module = undefined :: 'undefined' | atom(),
                      cli_fsm_module = undefined :: 'undefined' | atom(),
                      exec_checker_module = undefined :: 'undefined' | atom()}).

-define(BUFFER_START_FAIL_MESSAGE, "Buffer creation fails").
-define(UNSUITABLE_COMMAND_MESSAGE, "Unsuitable command").
-define(ACCESS_DENIED_MESSAGE, "Access denied").
-define(BAD_CONFIG_MESSAGE, "Bad authorization config").
-define(COMMAND_FAIL_MESSAGE, "Command fails with return code ").
