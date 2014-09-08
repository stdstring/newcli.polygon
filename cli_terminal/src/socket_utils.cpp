#include <cstring>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>

#include "exception_def.h"
#include "socket_utils.h"

namespace cli_terminal
{

int create_socket()
{
    int socketd = socket(AF_INET, SOCK_STREAM, 0);
    if (socketd == -1)
        throw socket_error();
    return socketd;
}

void connect(int socketd, int port_number)
{
    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(port_number);
    int ip_binary;
    inet_pton(AF_INET, IP_ADDRESS, &ip_binary);
    int convert_result = inet_pton(AF_INET, IP_ADDRESS, &ip_binary);
    if (convert_result != 1)
        throw socket_error();
    server_addr.sin_addr.s_addr = ip_binary;
    int connect_result = connect(socketd, reinterpret_cast<sockaddr*>(&server_addr), sizeof(server_addr));
    if (connect_result == -1)
        throw socket_error();
}

}