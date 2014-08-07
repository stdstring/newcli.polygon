%% cli_terminal message definitions:

-record(command, {command_line = "" :: string()}).
-record(command_out, {data = "" :: string()}).
-record(command_err, {data = "" :: string()}).
-record('end', {prompt = "" :: string()}).
-record(error, {reason = "" :: string()}).
-record(interrupt, {}).
-record(current_state_request, {}).
-record(current_state_response, {prompt = "" :: string()}).
-record(extension_request, {command_line = "" :: string()}).
-record(extension_response, {extension_list = [] :: [string()]}).
-record(exit, {}).