#ifndef H_SERVER_INTERACTION
#define H_SERVER_INTERACTION

#include <vector>

template <typename T> T read_message(int socketd);
template <typename T> std::vector<T> read_messages(int socketd);

template <typename T> void write_message(int socketd, T const &message);

template <typename TIn, typename TOut> TOut sync_exchange(int socketd, TIn const &request);

#endif