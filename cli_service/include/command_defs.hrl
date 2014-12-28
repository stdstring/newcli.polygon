%% command definitions

-define(ENTRY_MODULE_PREFIX, command_module_).
-define(ENTRY_FUNC, execute).
%%-record(command_exec_context, {user = undefined :: 'undefined' | #user{}, settings = undefined :: 'undefined' | dict()}).
-define(USER_KEY, user).
-define(SETTINGS_KEY, settings).