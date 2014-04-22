%% @author stdstring

-module(cli_frontend_application).

-include("common_defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-export([main/0, main/1]).

main() -> main("/tmp/frontend.conf").

main(MainConfigFile) ->
    GlobalConfig = config_reader:read(MainConfigFile),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec init_execution_state(GlobalConfig :: #global_config{}) -> #execution_state{}.
init_execution_state(GlobalConfig) ->
    ok.