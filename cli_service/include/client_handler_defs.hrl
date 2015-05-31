%% client handler definitions

%% login
-record(login, {username = "" :: string(), password = "" :: string()}).
-record(login_success, {user = undefined :: 'undefined' | #user{}, greeting = "" :: string()}).
-record(login_fail, {reason = "" :: string()}).
-record(login_error, {reason = "" :: string()}).
%% command management
-record(process_command, {command_line = "" :: string()}).
-record(interrupt_command, {}).
%% get_XXX info
-record(get_current_state, {}).
-record(get_extensions, {command_line = "" :: string()}).
-record(get_help, {command_line = "" :: string()}).
-record(get_suitable_commands, {command_line = "" :: string()}).
%% exit
-record(client_exit, {}).
-record(current_mode_exit, {}).
%% interaction from command
-record(command_output, {output = "" :: string()}).
-record(command_error, {error = "" :: string()}).
-record(finish_command, {return_code = 0 :: non_neg_integer(), exec_context = [] :: [{Key :: atom(), Value :: term()}]}).
-record(finish_exec, {return_code = 0 :: non_neg_integer(), exec_context = [] :: [{Key :: atom(), Value :: term()}]}).