%% common definitions

-record(cli_terminal_config, {}).
-record(global_config, {cli_terminal = #cli_terminal_config{} :: #cli_terminal_config{}}).