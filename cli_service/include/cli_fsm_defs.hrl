%% cli_fsm definitions:

%% TODO (std_string) : create records for states and transitions
-record(cli_fsm_config, {states = [] :: [{StateName :: atom(), Representation :: string(), [CommandName :: atom()], ExitCommand :: atom()}],
                         transitions = [] :: [{{FromStateName :: atom(), CommandName :: atom()}, ToStateName :: atom()}],
                         final_state = undefined :: 'undefined' | atom()}).
-record(cli_fsm_state, {config = #cli_fsm_config{} :: #cli_fsm_config{}, current_state = undefined :: 'undefined' | atom()}).
-record(cli_fsm_state_info, {current_state = undefined :: 'undefined' | atom(),
                             current_state_representation = "" :: string(),
                             commands = [] :: [CommandName :: atom()],
                             exit_command = undefined :: 'undefined' | atom()}).