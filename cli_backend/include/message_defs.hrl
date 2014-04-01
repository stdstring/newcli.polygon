%% message definitions:

-record(login, {login_name = "" :: string(), password = <<>> :: binary()}).
-record(logout, {}).

-record(command, {message = "" :: string()}).
-record(command_output, {message = "" :: string()}).
-record(command_error, {message = "" :: string()}).
-record(command_end, {completion_code = 0 :: integer(), cli_mode = "" :: string()}).
-record(command_fail, {reason = undefined :: 'undefined' | term()}).