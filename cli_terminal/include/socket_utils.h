#ifndef H_SOCKET_UTILS
#define H_SOCKET_UTILS

#include <string>

namespace cli_terminal
{

const std::string ip_address = "127.0.0.1";

int create_socket();
void connect(int socketd, int port_number);

}

#endif