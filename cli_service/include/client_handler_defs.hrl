%% client handler definitions

-define(PROCESS(CommandLine), {process_command, CommandLine}).
-define(INTERRUPT, interrupt_command).
-define(CURRENT_STATE, get_current_state).
-define(EXTENSIONS(CommandLine), {get_extensions, CommandLine}).
-define(EXIT, client_exit).
-define(CURRENT_MODE_EXIT, current_mode_exit).
-define(COMMAND_OUTPUT(Output), {command_output, Output}).
-define(COMMAND_ERROR(Error), {command_error, Error}).
-define(FINISH_COMMAND(ReturnCode, ExecutionContext), {finish_command, ReturnCode, ExecutionContext}).
-define(FINISH_EXEC(ReturnCode, ExecutionContext), {finish_exec, ReturnCode, ExecutionContext}).
-define(HELP(CommandLine), {help, CommandLine}).
-define(SUITABLE_COMMANDS(CommandLine), {suitable_commands, CommandLine}).
-define(LOGIN(Username, Password), {login, Username, Password}).