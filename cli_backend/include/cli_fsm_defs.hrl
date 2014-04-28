%% cli_fsm definitions:

-record(fsm_config, {states = [] :: [{StateName :: atom(), Representation :: string(), [CommandName :: atom()]}],
                     transitions = [] :: [{{FromStateName :: atom(), CommandName :: atom()}, ToStateName :: atom()}],
                     terminal_states = [] :: [StateName :: atom()]}).

-record(cli_fsm_state, {config = #fsm_config{} :: #fsm_config{}, current_state = unknown :: atom()}).