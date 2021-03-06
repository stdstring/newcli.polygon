#include <vector>
#include <cstring>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/uio.h>

#include "exception_def.h"
#include "server_interaction.h"

namespace cli_terminal
{

byte_array_ptr read_message(int socketd)
{
    int length_binary;
    ssize_t recv_length_result = recv(socketd, &length_binary, sizeof(int), MSG_WAITALL);
    if (-1 == recv_length_result)
        throw recv_error();
    int length = ntohl(length_binary);
    byte_array_ptr buffer(new unsigned char[length], length);
    ssize_t recv_data_result = recv(socketd, buffer.get(), length, MSG_WAITALL);
    if (-1 == recv_data_result)
        throw recv_error();
    return buffer;
}

bool contains_unread_data(int socketd)
{
    int data;
    ssize_t peek_result = recv(socketd, &data, sizeof(data), MSG_PEEK | MSG_DONTWAIT);
    return peek_result > 0;
}

void write_message(int socketd, byte_array_ptr const &serialized_data)
{
    int length_binary = htonl(serialized_data.size());
    struct iovec messages[2];
    // length message
    messages[0].iov_base = &length_binary;
    messages[0].iov_len = sizeof(length_binary);
    // data message
    messages[1].iov_base = serialized_data.get();
    messages[1].iov_len = serialized_data.size();
    // msghdr
    struct msghdr prepared_data;
    memset(&prepared_data, 0, sizeof(prepared_data));
    prepared_data.msg_iov = messages;
    prepared_data.msg_iovlen = 2;
    ssize_t send_result = sendmsg(socketd, &prepared_data, 0);
    if (-1 == send_result)
        throw send_error();
}

}