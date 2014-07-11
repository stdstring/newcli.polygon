// C++
#include <cstdlib>
#include <iostream>
// C
#include <strings.h>
// network
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>

#define IP_ADDRESS "127.0.0.1"
#define PORT 22222

int main()
{
    std::cout << "start terminal_work_example_client" << std::endl;
    std::cout << "finish terminal_work_example_client" << std::endl;
    return 0;
}

int create_socket()
{
    struct sockaddr_in addr;
    bzero(&addr, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(PORT);
    int ip_binary;
    if (inet_pton(AF_INET, IP_ADDRESS, &ip_binary) != 1)
    {
        std::exit(-1);
    }
    addr.sin_addr.s_addr = ip_binary;
    int socketd = socket(AF_INET, SOCK_STREAM, 0);
    if (socketd == -1)
    {
        std::exit(-1);
    }
    if (bind(socketd, (struct sockaddr*) (&addr), sizeof(addr)) == -1)
    {
        std::exit(-1);
    }
    return socketd;
}