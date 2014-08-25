-module(cli_frontend_testing).

-export([connect/1, send_login/3, send_command/2, send_current_state/1, send_extensions/2, receive_all/1]).

-include("cli_terminal_message_defs.hrl").

connect(PortNumber) ->
    Address = {127, 0, 0, 1},
    {ok, Socket} = gen_tcp:connect(Address, PortNumber, [binary, {packet, 4}]),
    Socket.

send_login(Socket, Login, Password) ->
    PasswordString = base64:encode_to_string(Password),
    CommandLine = "login " ++ Login ++ " " ++ PasswordString,
    Message = #command{command_line = CommandLine},
    send_message(Socket, Message).

send_command(Socket, CommandLine) ->
    Message = #command{command_line = CommandLine},
    send_message(Socket, Message).

send_current_state(Socket) ->
    Message = #current_state_request{},
    send_message(Socket, Message).

send_extensions(Socket, CommandLine) ->
    Message = #extension_request{command_line = CommandLine},
    send_message(Socket, Message).

receive_all(Socket) ->
    receive_all_impl(Socket, []).

send_message(Socket, Message) ->
    Data = term_to_binary(Message),
    gen_tcp:send(Socket, Data).

receive_all_impl(Socket, Messages) ->
    case receive_message(Socket) of
        {true, Message} -> receive_all_impl(Socket, Messages ++ [Message]);
        false -> Messages
    end.

receive_message(Socket) ->
    receive
        {tcp, Socket, Data} ->
            Message = binary_to_term(Data),
            {true, Message};
        {tcp_closed, Socket} ->
            false
    after 0 -> false
    end.