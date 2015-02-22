%% code generator definitions

-record(module_defs, {io_buffer_module = undefined :: 'undefined' | atom(),
                      client_handler_module = undefined :: 'undefined' | atom(),
                      cli_fsm_module = undefined :: 'undefined' | atom(),
                      exec_checker_module = undefined :: 'undefined' | atom()}).

-define(BUFFER_START_FAIL_MESSAGE, "Buffer creation fails\n").
-define(UNSUITABLE_COMMAND_MESSAGE, "Unsuitable command\n").
-define(ACCESS_DENIED_MESSAGE, "Access denied\n").
-define(BAD_CONFIG_MESSAGE, "Bad authorization config\n").
-define(COMMAND_FAIL_TEMPLATE, "Command execution failed. Return code is ~w\n").

-define(IO_BUFFER_MODULE(ModuleDefs), {atom, 0, ModuleDefs#module_defs.io_buffer_module}).
-define(CLIENT_HANDLER_MODULE(ModuleDefs), {atom, 0, ModuleDefs#module_defs.client_handler_module}).
-define(CLI_FSM_MODULE(ModuleDefs), {atom, 0, ModuleDefs#module_defs.cli_fsm_module}).