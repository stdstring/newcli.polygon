#ifndef H_SOCKET_UTILS
#define H_SOCKET_UTILS

#define IP_ADDRESS "127.0.0.1"

int create_socket();
void connect(int socketd, int port_number);

class socket_error
{};

#endif