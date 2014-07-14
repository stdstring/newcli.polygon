// C++
#include <algorithm>
#include <cstdlib>
#include <cstring>
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
// readline library
#include <readline.h>
#include <history.h>


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

struct ClientState
{
public:
    int socketd;
    bool allow_input;
    bool allow_running;
};

// typedefs
typedef std::unique_ptr<char, std::function<void (char*)>> cstr_unique_ptr;
typedef std::unique_ptr<ETERM, std::function<void (ETERM*)>> eterm_unique_ptr;

// init
void initialize();
// sockets
int create_socket();
void connect(int socketd);
// messages
Message read_message(int socketd);
void write_message(int socketd, std::string const &message);
ProcessResult process_message(Message const &message);
// readline
void readline_handler(char *raw_data);
// trim string
std::string trim_left(std::string const &source);
std::string trim_right(std::string const& source);
std::string trim_full(std::string const& source);

// cstr deleter
std::function<void (char*)> cstr_deleter = [](char* str){free(str);};
// eterm deleter
std::function<void (ETERM*)> eterm_deleter = [](ETERM* term){erl_free_term(term);};

// global variables
ClientState client_state;
const char *prompt = "readline usage example >>>";

int main()
{
    std::cout << "start terminal_work_example_client" << std::endl;
    initialize();
    rl_callback_handler_install(prompt, readline_handler);
    client_state.socketd = create_socket();
    connect(client_state.socketd);
    client_state.allow_running = true;
    client_state.allow_input = true;
    while (client_state.allow_running)
    {
        fd_set fds;
        FD_ZERO(&fds);
        FD_SET(STDIN_FILENO, &fds);
        FD_SET(client_state.socketd, &fds);
        int result = select(FD_SETSIZE, &fds, nullptr, nullptr, nullptr);
        if (result == -1)
        {
            if (errno != EINTR)
            {
                std::cout << "select error" << std::endl;
                rl_callback_handler_remove();
                client_state.allow_running = false;
            }
            continue;
        }
        if (FD_ISSET(STDIN_FILENO, &fds))
        {
            if (client_state.allow_input)
                rl_callback_read_char();
            else
            {
                char buffer;
                read(STDIN_FILENO, &buffer, 1);
            }
        }
        if (FD_ISSET(client_state.socketd, &fds))
        {
            Message message = read_message(client_state.socketd);
            ProcessResult result = process_message(message);
            client_state.allow_running = result.allow_running;
            client_state.allow_input = result.allow_input;
        }
    }
    std::cout << "finish terminal_work_example_client" << std::endl;
    close(client_state.socketd);
    rl_deprep_terminal();
    return 0;
}

void initialize()
{    
    // erl runtime
    erl_init(NULL, 0);
    // readline
    rl_attempted_completion_over = 1;
    // completion
    //rl_attempted_completion_function = completion_func;
    //rl_sort_completion_matches = 0;
    //rl_ignore_completion_duplicates = 0;
    /*// singnals
    rl_catch_signals = 0;
    rl_catch_sigwinch = 0;
    // absent in readline 6.2
    // rl_change_environment = 0;*/
     // readline history
    using_history();
}

int create_socket()
{
    int socketd = socket(AF_INET, SOCK_STREAM, 0);
    if (socketd == -1)
    {
        std::cout << "error in socket" << std::endl;
        rl_deprep_terminal();
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
        rl_deprep_terminal();
        std::exit(-1);
    }
    server_addr.sin_addr.s_addr = ip_binary;
    if (connect(socketd, reinterpret_cast<sockaddr*>(&server_addr), sizeof(server_addr)) == -1)
    {
        std::cout << "error in connect" << std::endl;
        rl_deprep_terminal();
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
        rl_deprep_terminal();
        std::exit(-1);
    }
    int length = ntohl(length_binary);
    // read body
    std::unique_ptr<unsigned char[]> buffer(new unsigned char[length]);
    if (recv(socketd, buffer.get(), length, MSG_WAITALL) != length)
    {
        std::cout << "error in read body" << std::endl;
        rl_deprep_terminal();
        std::exit(-1);
    }    
    // data deserialization
    eterm_unique_ptr message_body(erl_decode(buffer.get()), eterm_deleter);
    if (!ERL_IS_TUPLE(message_body.get()))
    {
        std::cout << "bad message body" << std::endl;
        rl_deprep_terminal();
        std::exit(-1);
    }
    eterm_unique_ptr message_type(erl_element(1, message_body.get()), eterm_deleter);
    if (message_type.get() == nullptr)
    {
        std::cout << "bad message type" << std::endl;
        rl_deprep_terminal();
        std::exit(-1);
    }
    eterm_unique_ptr message_data(erl_element(2, message_body.get()), eterm_deleter);
    if (message_data.get() == nullptr)
    {
        std::cout << "bad message data" << std::endl;
        rl_deprep_terminal();
        std::exit(-1);
    }
    std::string type(ERL_ATOM_PTR(message_type));
    cstr_unique_ptr data_str(erl_iolist_to_string(message_data.get()), cstr_deleter);
    std::string data(data_str.get());
    return Message(type, data);
}

void write_message(int socketd, std::string const &message)
{
    // data serialization
    eterm_unique_ptr message_body(erl_mk_string(message.c_str()), eterm_deleter);
    int length = erl_term_len(message_body.get());
    // buffer size = 4-byte length + term length
    int total_length = length + 4;
    std::unique_ptr<unsigned char[]> buffer(new unsigned char[total_length]);
    if (erl_encode(message_body.get(), (buffer.get() + 4)) != length)
    {
        std::cout << "error in message encode" << std::endl;
        rl_deprep_terminal();
        std::exit(-1);
    }
    int length_binary = htonl(length);
    memcpy(buffer.get(), &length_binary, 4);
    // write message
    if (send(socketd, buffer.get(), total_length, 0) != total_length)
    {
        std::cout << "error in write message" << std::endl;
        rl_deprep_terminal();
        std::exit(-1);
    }
}

ProcessResult process_message(Message const &message)
{
    std::string type = message.type;
    std::string data = message.data;
    if (type.compare("result") == 0)
    {
        std::cout << data;
        return ProcessResult(false, true);
    }
    if (type.compare("end") == 0)
    {
        rl_callback_handler_install(prompt, readline_handler);
        return ProcessResult(true, true);
    }
    if (type.compare("timeout") == 0)
    {
        std::cout << data;
        return ProcessResult(false, false);
    }
    return ProcessResult(true, true);
}

void readline_handler(char *raw_data)
{
    if (raw_data == nullptr)
    {
        std::cout << "exit handler" << std::endl;
        client_state.allow_running = false;
        rl_callback_handler_remove();
        return;
    }    
    cstr_unique_ptr raw_data_ptr(raw_data, cstr_deleter);
    std::string raw_str = std::string(raw_data_ptr.get());
    std::string line = trim_full(raw_str);
    if (line.empty())
        return;
    char *expansion = nullptr;
    int result = history_expand(const_cast<char*>(line.c_str()), &expansion);
    cstr_unique_ptr expansion_ptr(expansion, cstr_deleter);
    if (result == 0 || result == 1)
        add_history(expansion);
    std::string message(expansion_ptr.get());
    write_message(client_state.socketd, message);
    rl_callback_handler_remove();
    client_state.allow_input = false;
}

std::string trim_left(std::string const &source)
{
    std::string::const_iterator end = source.end();
    std::string::const_iterator new_begin = std::find_if(source.begin(), end, [](char c){return !std::isspace(c);});
    return new_begin != end ? std::string(new_begin, end) : std::string();
}

std::string trim_right(std::string const& source)
{
    std::string::const_reverse_iterator rend = source.rend();
    std::string::const_reverse_iterator new_rbegin = std::find_if(source.rbegin(), rend, [](char c){return !std::isspace(c);});
    return new_rbegin != rend ? std::string(source.begin(), new_rbegin.base()) : std::string();
}

std::string trim_full(std::string const& source)
{
    std::string trim_left_result = trim_left(source);
    return trim_right(trim_left_result);
}