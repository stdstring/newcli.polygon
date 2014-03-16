%% message definitions:

-record(command_output, {message = "" :: string()}).
-record(command_error, {message = "" :: string()}).
-record(command_end, {completion_code = 0 :: integer(), cli_mode = "" :: string()}).