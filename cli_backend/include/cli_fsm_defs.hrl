%% cli_fsm definitions:

-record(fsm_config, {states = [] :: [{StateName :: atom(), [CommandName :: atom()]}],
                     transitions = [] :: [{{FromStateName :: atom(), CommandName :: atom()}, ToStateName :: atom()}]}).
-record(cli_fsm_state, {config = #fsm_config{} :: #fsm_config{}, current_state = unknown :: atom()}).