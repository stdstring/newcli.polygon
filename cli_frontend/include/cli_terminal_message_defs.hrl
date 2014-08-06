%% cli_terminal message definitions:

-record(command, {command_line = "" :: string()}).
-record(command_out, {command_output = "" :: string()}).
-record(command_err, {command_error = "" :: string()}).
-record('end', {prompt = "" :: string()}).
-record(interrupt, {}).
-record(current_state_request, {}).
-record(current_state_response, {prompt = "" :: string()}).
-record(extension_request, {command_line = "" :: string()}).
-record(extension_response, {extension_list = [] :: [string()]}).