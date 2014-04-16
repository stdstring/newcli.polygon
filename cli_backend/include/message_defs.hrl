%% message definitions:

-record(login, {login_name = "" :: string(), password = <<>> :: binary()}).
-record(login_success, {session_pid = undefined :: 'undefined' | pid(), greeting = "" :: string()}).
-record(login_fail, {reason = undefined :: 'undefined' | term()}).

-record(logout, {}).
-record(logout_success, {}).

-record(commands_info, {}).
-record(command_info, {command_name :: atom(), command_body :: [string()], command_help :: string()}).
-record(commands_info_result, {info :: [#command_info{}]}).
-record(commands_info_fail, {reason = undefined :: 'undefined' | term()}).

-record(command, {message = "" :: string()}).
-record(command_output, {message = "" :: string()}).
-record(command_error, {message = "" :: string()}).
-record(command_end, {completion_code = 0 :: integer(), cli_mode = "" :: string()}).
-record(command_fail, {reason = undefined :: 'undefined' | term(), cli_mode = "" :: string()}).