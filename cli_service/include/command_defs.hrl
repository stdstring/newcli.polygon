%% command definitions

-define(ENTRY_MODULE_PREFIX, command_module).
-define(ENTRY_FUNC, execute).

%% for execution state
-define(USER_KEY, user).
-define(EX_STATE_KEY, execution_state).
-define(EX_CONTINUE, ex_continue).
-define(EX_STOP, ex_stop).