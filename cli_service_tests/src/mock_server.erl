%% @author std-string

-module(mock_server).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").
-include("mock_defs.hrl").

-export([start/0, stop/0, set_expected/1, execute/3, check_finish/0]).
%% gen_server export
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(mock_state, {expected = [] :: [#expectation{}]}).
-record(actual, {source = undefined :: 'undefined' | term(),
                 func = undefined :: 'undefined' | atom(),
                 args = [] :: [term()]}).

-define(SERVER_NAME, mock_server).

%% ====================================================================
%% API functions
%% ====================================================================

start() ->
    gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER_NAME, stop).

set_expected(Expected) ->
    gen_server:call(?SERVER_NAME, {expected, Expected}).

execute(Source, Func, Args) ->
    case gen_server:call(?SERVER_NAME, #actual{source = Source, func = Func, args = Args}) of
        {true, Result} -> Result;
        false ->
            ?debugFmt("call for source = \"~p\", func = \"~p\", args = ~p fails~n", [Source, Func, Args]),
            ?assert(false)
    end.

check_finish() ->
    case gen_server:call(?SERVER_NAME, finish) of
        [] -> ok;
        _Other -> ?assert(false)
    end.

init(_Args) ->
    {ok, #mock_state{}}.

handle_call({expected, Expected}, _From, State) ->
    {reply, true, State#mock_state{expected = Expected}};
handle_call(finish, _From, State) ->
    {reply, State#mock_state.expected, State};
handle_call(#actual{source = Source, func = Func, args = Args}, _From, #mock_state{expected = Expected}) ->
    case check_expectation(Expected, Source, Func, Args) of
        {true, Result, Rest} -> {reply, {true, Result}, #mock_state{expected = Rest}};
        false -> {reply, false, #mock_state{expected = []}}
    end.

handle_cast(stop, State) ->
    {stop, normal, State}.

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