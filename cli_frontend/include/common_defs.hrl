%% common definitions:

-record(global_config, {main_config_dir = "." :: string(), commands = []:: [{CommandName :: atom(), CommandModule :: atom()}]}).

-record(execution_state, {global_handler = undefined :: 'undefined' | atom(),
                          session = undefined :: 'undefined' | pid(),
                          commands_info = [] :: [{CommandName :: atom(), CommandBody :: [string()], CommandHelp :: string()}],
                          current_cli_mode :: atom()}).

-record(parse_result, {state = undefined :: 'undefined' | atom(),
                       command = undefined :: 'undefined' | {Name :: atom(), Module :: atom()},
                       can_continue = false :: boolean()}).