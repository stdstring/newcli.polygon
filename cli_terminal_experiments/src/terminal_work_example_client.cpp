// C++
#include <cstdlib>
#include <functional>
#include <iostream>
#include <memory>
#include <utility>
// C
#include <strings.h>
#include <unistd.h>
// network
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
// select
#include <sys/select.h>
// erlang terms
#include <erl_interface.h>


#define IP_ADDRESS "127.0.0.1"
#define PORT 22222

struct Message
{
public:
    Message(std::string const &type_value, std::string const &data_value) : type(type_value), data(data_value) {};

    std::string type;
    std::string data;
};

struct ProcessResult
{
public:
    ProcessResult(bool allow_input_value, bool allow_running_value) : allow_input(allow_input_value), allow_running(allow_running_value) {};

    bool allow_input;
    bool allow_running;    
};

// typedefs
typedef std::unique_ptr<ETERM, std::function<void (ETERM*)>> eterm_unique_ptr;

// declaration
void initialize();
int create_socket();
void connect(int socketd);
Message read_message(int socketd);
void write_message(int socketd, std::string const &message);
ProcessResult process_message(Message const &message);

// eterm deleter
std::function<void (ETERM*)> eterm_deleter = [](ETERM* term){erl_free_term(term);};

int main()
{
    std::cout << "start terminal_work_example_client" << std::endl;
    initialize();
    int socketd = create_socket();
    connect(socketd);
    bool running = true;
    bool allow_input = true;
    while (running)
    {
        fd_set fds;
        FD_ZERO(&fds);
        FD_SET(STDIN_FILENO, &fds);
        FD_SET(socketd, &fds);
        int result = select(FD_SETSIZE, &fds, nullptr, nullptr, nullptr);
        if (result == -1)
        {
            if (errno != EINTR)
            {
                std::cout << "select error" << std::endl;
                running = false;
            }
            continue;
        }
        if (FD_ISSET(STDIN_FILENO, &fds))
        {
            // some action
        }
        if (FD_ISSET(socketd, &fds))
        {
            Message message = read_message(socketd);
            ProcessResult result = process_message(message);
            running = result.allow_running;
            allow_input = result.allow_input;
        }
    }
    std::cout << "finish terminal_work_example_client" << std::endl;
    close(socketd);
    return 0;
}

void initialize()
{
    erl_init(NULL, 0);
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

Message read_message(int socketd)
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
    std::unique_ptr<unsigned char[]> buffer(new unsigned char[length]);
    if (recv(socketd, buffer.get(), length, MSG_WAITALL) != length)
    {
        std::cout << "error in read body" << std::endl;
        std::exit(-1);
    }    
    // data deserialization
    eterm_unique_ptr message_body(erl_decode(buffer.get()), eterm_deleter);
    if (!ERL_IS_TUPLE(message_body.get()))
    {
        std::cout << "bad message body" << std::endl;
        std::exit(-1);
    }
    eterm_unique_ptr message_type(erl_element(1, message_body.get()));
    if (message_type.get() == nullptr)
    {
        std::cout << "bad message type" << std::endl;
        std::exit(-1);
    }
    eterm_unique_ptr message_data(erl_element(2, message_body.get()));
    if (message_data.get() == nullptr)
    {
        std::cout << "bad message data" << std::endl;
        std::exit(-1);
    }
    std::string type(ERL_ATOM_PTR(message_type));
    std::unique_ptr<char[]> data_str(erl_iolist_to_string(message_data.get()));
    std::string data(data_str.get());
    return Message(type, data);
}

void write_message(int socketd, std::string const &message)
{}

ProcessResult process_message(Message const &message)
{
    std::string type = message.type;
    std::string data = message.data;
    if (type.compare("result"))
    {
        std::cout << data;
        return ProcessResult(false, true);
    }
    if (type.compare("end"))
    {
        return ProcessResult(true, true);
    }
    if (type.compare("timeout"))
    {
        std::cout << data;
        return ProcessResult(false, false);
    }
    return ProcessResult(true, true);
}