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
// poll
#include <poll.h>
// signal
#include <signal.h>
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
    Message(std::string const &type_value, std::string const &data_value) : type(type_value), data(data_value) {}

    std::string type;
    std::string data;
};

struct ProcessResult
{
public:
    ProcessResult(bool allow_input_value, bool allow_running_value) : allow_input(allow_input_value), allow_running(allow_running_value) {}

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

class SignalMaskHolder
{
public:
    // delete default members
    SignalMaskHolder() = delete;
    SignalMaskHolder(const SignalMaskHolder&) = delete;
    SignalMaskHolder(SignalMaskHolder&&) = delete;
    SignalMaskHolder& operator=(const SignalMaskHolder&) = delete;
    SignalMaskHolder& operator=(SignalMaskHolder&&) = delete;

    // definitions
    SignalMaskHolder(sigset_t new_mask)
    {
        sigprocmask(SIG_SETMASK, &new_mask, &_old_mask);
    }

    ~SignalMaskHolder()
    {
        sigprocmask(SIG_SETMASK, &_old_mask, nullptr);
    }
private:
    sigset_t _old_mask;
};

class SignalSafeExecuter
{
public:
    // delete default members
    SignalSafeExecuter() = delete;
    SignalSafeExecuter(const SignalSafeExecuter&) = delete;
    SignalSafeExecuter(SignalSafeExecuter&&) = delete;
    SignalSafeExecuter& operator=(const SignalSafeExecuter&) = delete;
    SignalSafeExecuter& operator=(SignalSafeExecuter&&) = delete;
    // definitions
    SignalSafeExecuter(sigset_t mask) : _mask(mask) {}
    ~SignalSafeExecuter() {}

    template<class Ret> Ret execute(std::function<Ret()> func)
    {
        SignalMaskHolder signalHolder(_mask);
        return func();
    }

    void execute(std::function<void()> func)
    {
        SignalMaskHolder signalHolder(_mask);
        func();
    }
private:
    sigset_t _mask;
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
//signals
sigset_t create_signal_mask();
void setup_signal_handlers();
void signal_handler(int signo);

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
    setup_signal_handlers();
    rl_callback_handler_install(prompt, readline_handler);
    client_state.socketd = create_socket();
    connect(client_state.socketd);
    client_state.allow_running = true;
    client_state.allow_input = true;
    sigset_t mask = create_signal_mask();
    SignalSafeExecuter executer(mask);
    // pollfd
    struct pollfd fdarray[2];
    fdarray[0].fd = STDIN_FILENO;
    fdarray[0].events = POLLIN;
    fdarray[1].fd = client_state.socketd;
    fdarray[1].events = POLLIN;
    while (client_state.allow_running)
    {
        fdarray[0].revents = 0;
        fdarray[1].revents = 0;
        int result = poll(fdarray, 2, -1);
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
        if ((fdarray[0].revents & POLLIN) == POLLIN)
        {
            if (client_state.allow_input)
                rl_callback_read_char();
        }
        if ((fdarray[0].revents & POLLERR) == POLLERR)
        {
            std::cout << "error in poll from STDIN_FILENO" << std::endl;
            rl_deprep_terminal();
            std::exit(-1);
        }
        if ((fdarray[1].revents & POLLIN) == POLLIN)
        {
            Message message = executer.execute<Message>([](){return read_message(client_state.socketd);});
            ProcessResult result = process_message(message);
            client_state.allow_running = result.allow_running;
            client_state.allow_input = result.allow_input;
        }
        if ((fdarray[1].revents & POLLERR) == POLLERR)
        {
            std::cout << "error in poll from socket" << std::endl;
            rl_deprep_terminal();
            std::exit(-1);
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
    // singnals
    rl_catch_signals = 0;
    rl_catch_sigwinch = 0;
    // absent in readline 6.2
    // rl_change_environment = 0;
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
        setup_signal_handlers();
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
    sigset_t mask = create_signal_mask();
    SignalSafeExecuter executer(mask);
    executer.execute([&message](){write_message(client_state.socketd, message);});
    rl_callback_handler_remove();
    setup_signal_handlers();
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

sigset_t create_signal_mask()
{
    sigset_t mask;
    sigemptyset(&mask);
    sigaddset(&mask, SIGINT);
    sigaddset(&mask, SIGQUIT);
    sigaddset(&mask, SIGTERM);
    sigaddset(&mask, SIGHUP);
    sigaddset(&mask, SIGALRM);
    sigaddset(&mask, SIGTSTP);
    sigaddset(&mask, SIGTTIN);
    sigaddset(&mask, SIGTTOU);
    sigaddset(&mask, SIGWINCH);
    return mask;
}

void setup_signal_handlers()
{
    sigset_t old_mask;
    sigset_t mask = create_signal_mask();
    if (pthread_sigmask(SIG_SETMASK, &mask, &old_mask) != 0)
    {
        std::cout << "error in pthread_sigmask" << std::endl;
        rl_deprep_terminal();
        std::exit(-1);
    }
    struct sigaction int_action;
    int_action.sa_handler = signal_handler;
    int_action.sa_mask = mask;
    if (sigaction(SIGINT, &int_action, nullptr) == -1)
    {
        std::cout << "error in sigaction(SIGINT, ...)" << std::endl;
        rl_deprep_terminal();
        std::exit(-1);
    }
    struct sigaction quit_action;
    quit_action.sa_handler = signal_handler;
    quit_action.sa_mask = mask;
    if (sigaction(SIGQUIT, &quit_action, nullptr) == -1)
    {
        std::cout << "error in sigaction(SIGQUIT, ...)" << std::endl;
        rl_deprep_terminal();
        std::exit(-1);
    }
    struct sigaction winch_action;
    winch_action.sa_handler = signal_handler;
    winch_action.sa_mask = mask;
    if (sigaction(SIGWINCH, &winch_action, nullptr) == -1)
    {
        std::cout << "error in sigaction(SIGWINCH, ...)" << std::endl;
        rl_deprep_terminal();
        std::exit(-1);
    }
    struct sigaction tstp_action;
    tstp_action.sa_handler = signal_handler;
    tstp_action.sa_mask = mask;
    if (sigaction(SIGTSTP, &tstp_action, nullptr) == -1)
    {
        std::cout << "error in sigaction(SIGTSTP, ...)" << std::endl;
        rl_deprep_terminal();
        std::exit(-1);
    }
    if (pthread_sigmask(SIG_SETMASK, &old_mask, nullptr) != 0)
    {
        std::cout << "error in pthread_sigmask" << std::endl;
        rl_deprep_terminal();
        std::exit(-1);
    }
}

void signal_handler(int signo)
{
    if (signo == SIGINT)
    {
        if (client_state.allow_input)
        {
            std::cout << "^C" << std::endl;
            rl_callback_handler_remove();
            rl_callback_handler_install(prompt, readline_handler);
            setup_signal_handlers();
        }
        else
        {
            std::cout << std::endl;
            write_message(client_state.socketd, "stop");
        }
    }
    if (signo == SIGQUIT)
    {
        if (client_state.allow_input)
            std::cout << "^\\" << std::endl;
        rl_callback_handler_remove();
        client_state.allow_running = false;
    }
    if (signo == SIGWINCH)
    {
        // ??
    }
    if (signo == SIGTSTP)
    {
        if (client_state.allow_input)
            std::cout << "^Z" << std::endl;
        rl_callback_handler_remove();
        client_state.allow_running = false;
    }
}
