-module(prompt_factory_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DEVICE_NAME, "CliDemo").

-record(login_info, {login_name = "" :: string(), is_admin = false :: boolean()}).

-record(execution_state, {global_handler = undefined :: 'undefined',
                          device_name = ?DEVICE_NAME :: string(),
                          session = undefined :: 'undefined',
                          login_info = undefined :: 'undefined' | #login_info{},
                          commands_info = [] :: [],
                          current_cli_mode = undefined :: 'undefined' | atom()}).


%% ====================================================================
%% Test functions
%% ====================================================================

generate_prompt_test_() ->
    [{"prompt for undefined cli mode and undefined login", ?_assertEqual("@CliDemo>", prompt_factory:generate_prompt(#execution_state{}))},
     {"prompt for empty cli mode and undefined login", ?_assertEqual("@CliDemo>", prompt_factory:generate_prompt(create_execution_state("")))},
     {"prompt for nonempty cli mode and undefined login", ?_assertEqual("@CliDemo (Some-Mode)>", prompt_factory:generate_prompt(create_execution_state("Some-Mode")))},
     {"prompt for undefined cli mode and user", ?_assertEqual("SimpleUser@CliDemo>", prompt_factory:generate_prompt(create_execution_state("SimpleUser", false)))},
     {"prompt for undefined cli mode and admin", ?_assertEqual("Root@CliDemo#", prompt_factory:generate_prompt(create_execution_state("Root", true)))},
     {"prompt for empty cli mode and user", ?_assertEqual("SimpleUser@CliDemo>", prompt_factory:generate_prompt(create_execution_state("", "SimpleUser", false)))},
     {"prompt for empty cli mode and admin", ?_assertEqual("Root@CliDemo#", prompt_factory:generate_prompt(create_execution_state("", "Root", true)))},
     {"prompt for nonempty cli mode and user", ?_assertEqual("SimpleUser@CliDemo (Some-Mode)>", prompt_factory:generate_prompt(create_execution_state("Some-Mode", "SimpleUser", false)))},
     {"prompt for nonempty cli mode and admin", ?_assertEqual("Root@CliDemo (Some-Mode)#", prompt_factory:generate_prompt(create_execution_state("Some-Mode", "Root", true)))}].

create_execution_state(CliMode) ->
    #execution_state{current_cli_mode = CliMode}.

create_execution_state(Login, IsAdmin) ->
    LoginInfo = #login_info{login_name = Login, is_admin = IsAdmin},
    #execution_state{login_info = LoginInfo}.

create_execution_state(CliMode, Login, IsAdmin) ->
    LoginInfo = #login_info{login_name = Login, is_admin = IsAdmin},
    #execution_state{current_cli_mode = CliMode, login_info = LoginInfo}.