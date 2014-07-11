// C++
#include <cstdlib>
#include <iostream>
#include <utility>
// C
#include <strings.h>
// network
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>

#define IP_ADDRESS "127.0.0.1"
#define PORT 22222

// declaration
int create_socket();
void connect(int socketd);
std::pair<std::string, std::string> read_message(int socketd);
void write_message(int socketd, std::string const &message);

// global variables
bool running = true;

int main()
{
    std::cout << "start terminal_work_example_client" << std::endl;
    int socketd = create_socket();
    connect(socketd);
    while (running)
    {
        
    }
    std::cout << "finish terminal_work_example_client" << std::endl;
    return 0;
}

int create_socket()
{
    int socketd = socket(AF_INET, SOCK_STREAM, 0);
    if (socketd == -1)
    {
        std::cout << "error in socket" << std::endl;
        std::exit(-1);
    }
    return socketd;
}

void connect(int socketd)
{
    struct sockaddr_in server_addr;
    bzero(&server_addr, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(PORT);
    int ip_binary;
    if (inet_pton(AF_INET, IP_ADDRESS, &ip_binary) != 1)
    {
        std::cout << "error in inet_pton" << std::endl;
        std::exit(-1);
    }
    server_addr.sin_addr.s_addr = ip_binary;
    if (connect(socketd, reinterpret_cast<sockaddr*>(&server_addr), sizeof(server_addr)) == -1)
    {
        std::cout << "error in connect" << std::endl;
        std::exit(-1);
    }
}

std::pair<std::string, std::string> read_message(int socketd)
{
    // read 4-bytes length
    int length_binary;
    if (recv(socketd, &length_binary, sizeof(int), MSG_WAITALL) != sizeof(int))
    {
        std::cout << "error in read 4-bytes length" << std::endl;
        std::exit(-1);
    }
    int length = ntohl(length_binary);
    // read body
    char *buffer = new char[length];
    if (recv(socketd, buffer, length, MSG_WAITALL) != length)
    {
        std::cout << "error in read body" << std::endl;
        std::exit(-1);
    }
    delete[] buffer;
    // data deserialization
    // ...
    return std::pair<std::string, std::string>("", "");
}

void write_message(int socketd, std::string const &message)
{}