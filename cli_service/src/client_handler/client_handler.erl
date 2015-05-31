%% @author std-string

-module(client_handler).

-include("authentication_defs.hrl").
-include("client_handler_defs.hrl").
-include("common_defs.hrl").

-export([start/3]).
-export([login/3,
         process_command/2,
         interrupt_command/1,
         get_current_state/1,
         get_extensions/2,
         get_help/2,
         get_suitable_commands/2,
         exit/1,
         current_mode_exit/1]).
%% TODO (std_string) : may be move these into another place
-export([send_output/2, send_error/2, finish_command/3, finish_exec/3]).

%% ====================================================================
%% API functions
%% ====================================================================

%%-spec start(GlobalConfig :: #global_config{}, Endpoint :: pid(), SocketInfo :: {Address :: tuple(), Port :: pos_integer()}) ->
-spec start(GlobalConfig :: #global_config{}, Endpoint :: pid(), SocketInfo :: tuple()) ->
    {'ok', Pid :: pid()} | {'error', Reason :: term()}.
start(GlobalConfig, Endpoint, SocketInfo) ->
    gen_server:start_link(client_handler_unauth_impl, {GlobalConfig, Endpoint, SocketInfo}, []).

-spec login(Handler :: pid(), Username :: string(), Password :: string()) ->
    #login_success{} | #login_fail{} | #login_error{}.
login(Handler, Username, Password) ->
    gen_server:call(Handler, #login{username = Username, password = Password}).

-spec process_command(Handler :: pid(), CommandLine :: string()) -> boolean().
process_command(Handler, CommandLine) ->
    gen_server:call(Handler, #process_command{command_line = CommandLine}).

-spec interrupt_command(Handler :: pid()) -> 'ok'.
interrupt_command(Handler) ->
    gen_server:cast(Handler, #interrupt_command{}).

-spec get_current_state(Handler :: pid()) -> string().
get_current_state(Handler) ->
    gen_server:call(Handler, #get_current_state{}).

-spec get_extensions(Handler :: pid(), CommandLine :: string()) -> [string()].
get_extensions(Handler, CommandLine) ->
    gen_server:call(Handler, #get_extensions{command_line = CommandLine}).

-spec exit(Handler :: pid()) -> 'ok'.
exit(Handler) ->
    gen_server:cast(Handler, #client_exit{}).

-spec current_mode_exit(Handler :: pid()) -> term().
current_mode_exit(Handler) ->
    gen_server:call(Handler, #current_mode_exit{}).

-spec get_help(Handler :: pid(), CommandLine :: string()) -> string().
get_help(Handler, CommandLine) ->
    gen_server:call(Handler, #get_help{command_line = CommandLine}).

-spec get_suitable_commands(Handler :: pid(), CommandLine :: string()) -> [string()].
get_suitable_commands(Handler, CommandLine) ->
    gen_server:call(Handler, #get_suitable_commands{command_line = CommandLine}).

%% TODO (std_string) : may be move these into another place
-spec send_output(Handler :: pid(), Output :: string()) -> 'ok'.
send_output(Handler, Output) ->
    gen_server:cast(Handler, #command_output{output = Output}).

-spec send_error(Handler :: pid(), Error :: string()) -> 'ok'.
send_error(Handler, Error) ->
    gen_server:cast(Handler, #command_error{error = Error}).

-spec finish_command(Handler :: pid(), ReturnCode :: integer(), ExecutionState :: [{Key :: atom(), Value :: term()}]) -> 'ok'.
finish_command(Handler, ReturnCode, ExecutionState) ->
    gen_server:cast(Handler, #finish_command{return_code = ReturnCode, exec_context = ExecutionState}).

-spec finish_exec(Handler :: pid(), ReturnCode :: integer(), ExecutionState :: [{Key :: atom(), Value :: term()}]) -> 'ok'.
finish_exec(Handler, ReturnCode, ExecutionState) ->
    gen_server:cast(Handler, #finish_exec{return_code = ReturnCode, exec_context = ExecutionState}).


%% ====================================================================
%% Internal functions
%% ====================================================================