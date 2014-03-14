%% cli_fsm definitions:

-define(CONFIG_KEY, cli_fsm).
-define(DATA_SOURCE, data_source).

-record(fsm_config, {states = [] :: [{StateName :: atom(), [CommandName :: atom()]}],
                     transitions = [] :: [{{FromStateName :: atom(), CommandName :: atom()}, ToStateName :: atom()}]}).
-record(cli_fsm_state, {config = #fsm_config{} :: #fsm_config{}, current_state = unknown :: atom()}).