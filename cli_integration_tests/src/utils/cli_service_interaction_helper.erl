%% @author std-string

-module(cli_service_interaction_helper).

-export([sync_exchange_single_response/2, sync_exchange_multiple_response/3]).
-export([login/3, simple_logout/1]).

-include("message_defs.hrl").

-define(RESPONSE_TAG_INDEX, 1).

%% ====================================================================
%% API functions
%% ====================================================================

-spec login(Socket :: gen_tcp:socket(), Username :: string(), Password :: string()) -> tuple().
login(Socket, Username, Password) ->
    LoginRequest = ?LOGIN_REQUEST(Username, base64:encode_to_string(Password)),
    cli_service_interaction_helper:sync_exchange_single_response(Socket, LoginRequest).

-spec simple_logout(Socket :: gen_tcp:socket()) -> 'ok'.
simple_logout(Socket) ->
    cli_service_interaction_helper:sync_exchange_multiple_response(Socket, ?COMMAND_START("logout"), ?COMMAND_STOP_TAG),
    ok.

-spec sync_exchange_single_response(Socket :: gen_tcp:socket(), Request :: tuple()) -> tuple().
sync_exchange_single_response(Socket, Request) ->
    RequestBinary = term_to_binary(Request),
    gen_tcp:send(Socket, RequestBinary),
    {ok, ResponseBinary} = gen_tcp:recv(Socket, 0),
    binary_to_term(ResponseBinary).

-spec sync_exchange_multiple_response(Socket :: gen_tcp:socket(), Request :: tuple(), FinishResponseTag :: atom()) -> [tuple()].
sync_exchange_multiple_response(Socket, Request, FinishResponseTag) ->
    RequestBinary = term_to_binary(Request),
    gen_tcp:send(Socket, RequestBinary),
    receive_multiple_responses(Socket, FinishResponseTag, []).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec receive_multiple_responses(Socket :: gen_tcp:socket(), FinishResponseTag :: atom(), Result :: [tuple()]) -> [tuple()].
receive_multiple_responses(Socket, FinishResponseTag, Result) ->
    {ok, ResponseBinary} = gen_tcp:recv(Socket, 0),
    Response = binary_to_term(ResponseBinary),
    case element(?RESPONSE_TAG_INDEX, Response) of
        FinishResponseTag -> lists:reverse([Response] ++ Result);
        _Other -> receive_multiple_responses(Socket, FinishResponseTag, [Response] ++ Result)
    end.