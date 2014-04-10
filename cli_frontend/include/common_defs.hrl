%% common definitions:

-record(global_config, {main_config_dir = "." :: string(), commands = []:: [{CommandName :: atom(), CommandModule :: atom()}]}).

-record(execution_state, {global_handler = undefined :: 'undefined' | atom(), session = undefined :: 'undefined' | pid()}).
