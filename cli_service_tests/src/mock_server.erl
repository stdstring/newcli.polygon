%% @author std-string

-module(mock_server).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

-export([start/1, stop/0, execute/3]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(expectation, {source = undefined :: 'undefined' | term(),
                      func = undefined :: 'undefined' | atom(),
                      args = [] :: ['any' | term()],
                      result = undefined :: 'undefined' | term()}).
-record(mock_state, {expected = [] :: [#expectation{}]}).
-record(actual, {source = undefined :: 'undefined' | term(),
                 func = undefined :: 'undefined' | atom(),
                 args = [] :: [term()]}).

-define(SERVER_NAME, {local, mock_server}).

%% ====================================================================
%% API functions
%% ====================================================================

start(Expectations) ->
    gen_server:start_link(?SERVER_NAME, ?MODULE, Expectations, []).

stop() ->
    case gen_server:call(?SERVER_NAME, stop) of
        [] -> ok;
        _Other -> ?assert(false)
    end.

execute(Source, Func, Args) ->
    case gen_server:call(?SERVER_NAME, #actual{source = Source, func = Func, args = Args}) of
        {true, Result} -> Result;
        false -> ?assert(false)
    end.

init(_Args) ->
    ?assert(false).

handle_call(stop, _From, #mock_state{expected = Expected}) ->
    {stop, terminate, Expected};
handle_call(#actual{source = Source, func = Func, args = Args}, _From, #mock_state{expected = Expected}) ->
    case check_expectation(Expected, Source, Func, Args) of
        {true, Result, Rest} -> {reply, {true, Result}, #mock_state{expected = Rest}};
        false -> {reply, false, #mock_state{expected = []}}
    end.

handle_cast(_Request, State) ->
    {stop, enotsup, State}.

handle_info(_Info, State) ->
    {stop, enotsup, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

check_expectation([], _Source, _ActualFunc, _ActualArgs) -> false;
check_expectation([#expectation{source = Source, func = Func, args = Args, result = Result} | Rest], Source, Func, ActualArgs) ->
    case compare_args(Args, ActualArgs) of
        true -> {true, Result, Rest};
        false -> false
    end;
check_expectation(_Expected, _Source, _ActualFunc, _ActualArgs) -> false.

compare_args([], []) -> true;
compare_args(_ExpectedRest, []) -> false;
compare_args([], _ActualRest) -> false;
compare_args([Arg | ExpectedRest], [Arg | ActualRest]) -> compare_args(ExpectedRest, ActualRest);
compare_args([any | ExpectedRest], [_Arg | ActualRest]) -> compare_args(ExpectedRest, ActualRest);
compare_args(_Expected, _Actual) -> false.