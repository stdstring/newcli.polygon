#ifndef H_SERVER_INTERACTION
#define H_SERVER_INTERACTION

#include <vector>
#include "message_serialization.h"

namespace cli_terminal
{

byte_array_ptr read_message(int socketd);
bool contains_unread_data(int socketd);
void write_message(int socketd, byte_array_ptr const &serialized_data);

template <typename T> T read_message(int socketd)
{
    byte_array_ptr buffer = read_message(socketd);
    return deserialize<T>(buffer);
}

template <typename T> std::vector<T> read_messages(int socketd)
{
    std::vector<T> message_buffer;
    while (contains_unread_data(socketd))
        message_buffer.push_back(read_message<T>(socketd));
    return message_buffer;
}

template <typename T> void write_message(int socketd, T const &message)
{
    byte_array_ptr serialized_data = serialize(message);
    write_message(socketd, serialized_data);
}

template <typename TIn, typename TOut> TOut sync_exchange(int socketd, TIn const &request)
{
    write_message(socketd, request);
    return read_message<TOut>(socketd);
}

}

#endif