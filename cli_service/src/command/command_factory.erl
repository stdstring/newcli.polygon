%% @author std-string

-module(command_factory).

-export([process/5]).

-include("frame_defs.hrl").
-include("lexical_defs.hrl").
-include("syntax_defs.hrl").
-include("authentication_defs.hrl").
-include("command_defs.hrl").
-include("common_defs.hrl").

-define(MODULE_DEFS, #module_defs{io_buffer_module = io_buffer,
                                  client_handler_module = client_handler,
                                  cli_fsm_module = cli_fsm,
                                  exec_checker_module = command_execution_checker}).

%% ====================================================================
%% API functions
%% ====================================================================

-spec process(Source :: string(),
              LexConfig :: #lex_analyzer_config{},
              SyntaxConfig :: #syntax_analyzer_config{},
              GlobalConfig :: #global_config{},
              EntryModuleName :: atom()) ->
    {'true', Fun :: fun((CliFsm :: pid(), ClientHandler :: pid(), Context :: [{atom(), term()}]) -> 'ok')} | {'false', Reason :: term()}.
process(Source, LexConfig, SyntaxConfig, GlobalConfig, EntryModuleName) ->
    case command_parser:process(Source, LexConfig, SyntaxConfig) of
        {true, #command{} = Command} ->
            CommandName = Command#command.name,
            CommandModule = command_search:search_by_name(CommandName, GlobalConfig),
            UpdatedCommand = Command#command{module = CommandModule},
            case code_generator:generate(EntryModuleName, ?ENTRY_FUNC, UpdatedCommand, ?MODULE_DEFS) of
                {true, Binary} -> {true, create_remote_fun(EntryModuleName, Binary)};
                {false, Reason} -> {false, Reason}
            end;
        {false, Reason} -> {false, Reason}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec create_remote_fun(ModuleName :: atom(), Binary :: binary()) ->
    fun((CliFsm :: pid(), ClientHandler :: pid(), Context :: [{atom(), term()}]) -> 'ok').
create_remote_fun(ModuleName, Binary) ->
    fun(CliFsm, ClientHandler, Context) ->
        {module, ModuleName} = code:load_binary(ModuleName, [], Binary),
        apply(ModuleName, ?ENTRY_FUNC, [CliFsm, ClientHandler, Context]),
        ok
    end.