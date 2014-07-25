#include <cstring>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include "message_serialization.h"
#include "server_interaction.h"

void write_message(int socketd, byte_array_ptr serialized_data)
{
    int length_binary = htonl(serialized_data.size());
    struct iovec length_msg, data_msg;
    // length message
    memset(&length_msg, 0, sizeof(length_msg));
    length_msg.iov_base = &length_binary;
    length_msg.iov_len = sizeof(length_binary);
    // data message
    memset(&data_msg, 0, sizeof(data_msg));
    data_msg.iov_base = serialized_data.get();
    data_msg.iov_len = serialized_data.size();
    // msghdr
    struct msghdr prepared_data;
    memset(&prepared_data, 0, sizeof(prepared_data));
    struct iovec parts[] = {length_msg, data_msg};
    prepared_data.msg_iov = parts;
    prepared_data.msg_iovlen = 2;
    ssize_t send_result = sendmsg(socketd, &prepared_data, 0);
    if (-1 == send_result)
        throw send_error();
}

template <typename T> void write_message(int socketd, T const &message)
{
    byte_array_ptr serialized_data = serialize(message);
    write_message(socketd, serialized_data);
}