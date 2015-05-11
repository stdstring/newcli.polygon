%% config definitions

-define(COMMANDS_CONFIG_KEY, commands).
-define(COMMANDS_DATA_SOURCE, data_source).

-define(CLI_FSM_CONFIG_KEY, cli_fsm).
-define(CLI_FSM_DATA_SOURCE, data_source).
-define(CLI_FSM_STATES_KEY, states).
-define(CLI_FSM_TRANSITIONS_KEY, transitions).
-define(CLI_FSM_INIT_STATE_KEY, initial_state).
-define(CLI_FSM_FINAL_STATE_KEY, final_state).

-define(TERMINAL_CONFIG_KEY, cli_terminal).
-define(TERMINAL_PORT_NUMBER, port_number).
-define(TERMINAL_DOWNTIME, downtime).
-define(TERMINAL_LOGIN_COUNT, login_attempt_count).

%% default values
-define(TERMINAL_DOWNTIME_DEFAULT, 1).
-define(TERMINAL_LOGIN_COUNT_DEFAULT, 1).