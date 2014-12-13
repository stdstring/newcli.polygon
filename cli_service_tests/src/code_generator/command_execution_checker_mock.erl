%% @author std-string

-module(command_execution_checker_mock).

%%-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([]).

%%-export([start/2, execution_precheck/3]).
%% gen_server export
%%-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%-record(mock_state, {command = undefined :: 'undefined' | atom(),
%%                     result = true :: 'true' | {'false', Reason :: atom()},
%%                     trace = [] :: [tuple()]}).

%% ====================================================================
%% API functions
%% ====================================================================

%%start(Command, Result) ->
%%    gen_server:start_link(?MODULE, {Command, Result}, []).

%%execution_precheck(CommandName, CliFsm, User) ->
%%    gen_server:call(Buffer, {execution_precheck, CommandName}).

%%init({Command, Result}) ->
%%    {ok, #mock_state{command = Command, result = Result}}.

%%handle_call({execution_precheck, CommandName}, _From, State) ->
%%    ?assertEqual(State#mock_state.command, CommandName),
%%    case State#mock_state.trace of
%%        [] -> {reply, State#mock_state.result, State#mock_state{trace = [{execution_precheck, CommandName}]}};
%%        _Other -> ?assert(false)
%%    end;
%%handle_call(_Request, _From, _State) ->
%%    ?assert(false).

%%handle_cast(_Request, _State) ->
%%    ?assert(false).

%%handle_info(_Info, _State) ->
%%    ?assert(false).

%%terminate(_Reason, _State) -> ok.

%%code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================