%% code generator definitions

-record(module_defs, {io_buffer_module = undefined :: 'undefined' | atom(),
                      client_handler_module = undefined :: 'undefined' | atom(),
                      cli_fsm_module = undefined :: 'undefined' | atom(),
                      exec_checker_module = undefined :: 'undefined' | atom()}).