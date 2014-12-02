%% @author stdstring

-module(cli_fsm_behaviour).

-callback start(Config :: [tuple()]) -> pid().

-callback process_command(FsmPid :: pid(), CommandName :: atom()) -> StateName :: atom().

-callback get_current_state(FsmPid :: pid()) -> {StateName :: atom(), [CommandName :: atom()]}.