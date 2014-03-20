%% command definitions:

-record(command_state, {command_line = [] :: [string()],
                        stdout = undefined :: 'undefined' | pid(),
                        stderr = undefined :: 'undefined' | pid(),
                        additional_params = [] :: [{Key :: atom(), Value :: term()}]}).