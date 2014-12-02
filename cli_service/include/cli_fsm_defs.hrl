%% cli_fsm definitions:

-record(cli_fsm_config, {states = [] :: [{StateName :: atom(), Representation :: string(), [CommandName :: atom()]}],
                         transitions = [] :: [{{FromStateName :: atom(), CommandName :: atom()}, ToStateName :: atom()}]}).
-record(cli_fsm_state, {config = #cli_fsm_config{} :: #cli_fsm_config{}, current_state = unknown :: 'unknown' | atom()}).
-record(cli_fsm_state_info, {current_state = undefined :: atom(), current_state_representation :: atom(), commands = [] :: [CommandName :: atom()]}).