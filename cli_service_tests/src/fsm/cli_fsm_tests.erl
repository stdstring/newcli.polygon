%% @author std-string

-module(cli_fsm_tests).

-include("cli_fsm_defs.hrl").

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

process_command_test_() ->
    [{"process ping", process_commands([ping], [fundamental_mode])},
     {"process config_terminal, ping", process_commands([config_terminal, ping], [global_config_mode, global_config_mode])},
     {"process config_terminal, ping, exit", process_commands([config_terminal, ping, exit], [global_config_mode, global_config_mode, fundamental_mode])},
     {"process config_terminal, ping, end", process_commands([config_terminal, ping, 'end'], [global_config_mode, global_config_mode, fundamental_mode])},
     {"process interface", process_commands([interface], [fundamental_mode])},
     {"process config_terminal, interface", process_commands([config_terminal, interface], [global_config_mode, interface_config_mode])},
     {"process config_terminal, interface, exit", process_commands([config_terminal, interface, exit], [global_config_mode, interface_config_mode, global_config_mode])},
     {"process config_terminal, interface, end", process_commands([config_terminal, interface, 'end'], [global_config_mode, interface_config_mode, fundamental_mode])},
     {"process logout", process_commands([logout], [final_mode])},
     {"process config_terminal, exit, logout", process_commands([config_terminal, exit, logout], [global_config_mode, fundamental_mode, final_mode])}].

process_command_final_state_test() ->
    SourceData = create_source_data(),
    {ok, CliFsm} = cli_fsm:start(SourceData),
    process_flag(trap_exit, true),
    %% TODO (std_string) : think about supressing error output
    lists:foreach(fun(Command) -> cli_fsm:process_command(CliFsm, Command) end, [logout, ping]),
    process_flag(trap_exit, false),
    ?assertEqual([{'EXIT', CliFsm, final_state}], message_reader:read_all_messages()).

process_command_check_info_test_() ->
    [{"process ping",
      process_commands_check_info([ping], fundamental_mode, "", [config_terminal, ping, show_vlan, logout], logout)},
     {"process config_terminal",
      process_commands_check_info([config_terminal], global_config_mode, "config", [interface, interface_range, vlan, novlan, 'end', exit, show_vlan], exit)},
     {"process config_terminal, interface",
      process_commands_check_info([config_terminal, interface], interface_config_mode, "config-if", [switchport_vlan, noswitchport_vlan, 'end', exit, show_vlan], exit)},
     {"process config_terminal, interface, show_vlan",
      process_commands_check_info([config_terminal, interface, show_vlan], interface_config_mode, "config-if", [switchport_vlan, noswitchport_vlan, 'end', exit, show_vlan], exit)},
     {"process config_terminal, interface, exit",
      process_commands_check_info([config_terminal, interface, exit], global_config_mode, "config", [interface, interface_range, vlan, novlan, 'end', exit, show_vlan], exit)},
     {"process config_terminal, interface, end",
      process_commands_check_info([config_terminal, interface, 'end'], fundamental_mode, "", [config_terminal, ping, show_vlan, logout], logout)},
     {"process logout",
      process_commands_check_info([logout], final_mode, "", [], undefined)},
     {"process config_terminal, exit, logout",
      process_commands_check_info([config_terminal, exit, logout], final_mode, "", [], undefined)}].

get_current_state_test_() ->
    [{"process ping",
      process_current_state_info([ping], fundamental_mode, "", [config_terminal, ping, show_vlan, logout], logout)},
     {"process config_terminal",
      process_current_state_info([config_terminal], global_config_mode, "config", [interface, interface_range, vlan, novlan, 'end', exit, show_vlan], exit)},
     {"process config_terminal, interface",
      process_current_state_info([config_terminal, interface], interface_config_mode, "config-if", [switchport_vlan, noswitchport_vlan, 'end', exit, show_vlan], exit)},
     {"process config_terminal, interface, show_vlan",
      process_current_state_info([config_terminal, interface, show_vlan], interface_config_mode, "config-if", [switchport_vlan, noswitchport_vlan, 'end', exit, show_vlan], exit)},
     {"process config_terminal, interface, exit",
      process_current_state_info([config_terminal, interface, exit], global_config_mode, "config", [interface, interface_range, vlan, novlan, 'end', exit, show_vlan], exit)},
     {"process config_terminal, interface, end",
      process_current_state_info([config_terminal, interface, 'end'], fundamental_mode, "", [config_terminal, ping, show_vlan, logout], logout)},
     {"process logout",
      process_current_state_info([logout], final_mode, "", [], undefined)},
     {"process config_terminal, exit, logout",
      process_current_state_info([config_terminal, exit, logout], final_mode, "", [], undefined)}].

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_commands(CommandNames :: atom(), ExpectedStates :: atom()) -> term().
process_commands(CommandNames, ExpectedStates) ->
    SourceData = create_source_data(),
    {ok, CliFsm} = cli_fsm:start(SourceData),
    ActualStates = lists:map(fun(Command) -> Info = cli_fsm:process_command(CliFsm, Command), Info#cli_fsm_state_info.current_state end, CommandNames),
    ?_assertEqual(ExpectedStates, ActualStates).

-spec process_commands_check_info(CommandNames :: atom(),
                                  CurrentState :: atom(),
                                  CurrentStateRepr :: string(),
                                  AllowedCommands :: [atom()],
                                  ExitCommand :: atom()) -> term().
process_commands_check_info(CommandNames, CurrentState, CurrentStateRepr, AllowedCommands, ExitCommand) ->
    SourceData = create_source_data(),
    {ok, CliFsm} = cli_fsm:start(SourceData),
    lists:foldl(fun(Command, _Info) -> cli_fsm:process_command(CliFsm, Command) end, #cli_fsm_state_info{}, CommandNames),
    ExpectedInfo = #cli_fsm_state_info{current_state = CurrentState,
                                       current_state_representation = CurrentStateRepr,
                                       commands = AllowedCommands,
                                       exit_command = ExitCommand},
    ActualInfo = cli_fsm:get_current_state(CliFsm),
    ?_assertEqual(ExpectedInfo, ActualInfo).

-spec process_current_state_info(CommandNames :: atom(),
                                 CurrentState :: atom(),
                                 CurrentStateRepr :: string(),
                                 AllowedCommands :: [atom()],
                                 ExitCommand :: atom()) -> term().
process_current_state_info(CommandNames, CurrentState, CurrentStateRepr, AllowedCommands, ExitCommand) ->
    SourceData = create_source_data(),
    {ok, CliFsm} = cli_fsm:start(SourceData),
    lists:foreach(fun(Command) -> cli_fsm:process_command(CliFsm, Command) end, CommandNames),
    ExpectedInfo = #cli_fsm_state_info{current_state = CurrentState,
                                       current_state_representation = CurrentStateRepr,
                                       commands = AllowedCommands,
                                       exit_command = ExitCommand},
    ActualInfo = cli_fsm:get_current_state(CliFsm),
    ?_assertEqual(ExpectedInfo, ActualInfo).

create_source_data() ->
    [{states, [{fundamental_mode, "", [config_terminal, ping, show_vlan, logout], logout},
               {global_config_mode, "config", [interface, interface_range, vlan, novlan, 'end', exit, show_vlan], exit},
               {interface_config_mode, "config-if", [switchport_vlan, noswitchport_vlan, 'end', exit, show_vlan], exit},
               {interface_range_config_mode, "config-if-range", [switchport_vlan, noswitchport_vlan, 'end', exit, show_vlan], exit},
               {vlan_config_mode, "config-vlan", [name, noname, vlan, novlan, 'end', exit, show_vlan], exit},
               {final_mode, "", [], undefined}]},
     {transitions, [{fundamental_mode, global_config_mode, config_terminal},
                    {global_config_mode, interface_config_mode, interface},
                    {global_config_mode, interface_range_config_mode, interface_range},
                    {global_config_mode, vlan_config_mode, vlan},
                    {global_config_mode, vlan_config_mode, novlan},
                    {vlan_config_mode, global_config_mode, exit},
                    {interface_range_config_mode, global_config_mode, exit},
                    {interface_config_mode, global_config_mode, exit},
                    {global_config_mode, fundamental_mode, exit},
                    {vlan_config_mode, fundamental_mode, 'end'},
                    {interface_range_config_mode, fundamental_mode, 'end'},
                    {interface_config_mode, fundamental_mode, 'end'},
                    {global_config_mode, fundamental_mode, 'end'},
                    {fundamental_mode, final_mode, logout}]},
     {initial_state, fundamental_mode},
     {final_state, final_mode}].