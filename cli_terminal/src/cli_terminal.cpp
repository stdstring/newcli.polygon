#include <array>
#include <exception>
#include <functional>
#include <iostream>
#include <cstddef>
#include <string>
#include <unordered_map>
#include <vector>

#include <erl_interface.h>
#include <history.h>
#include <poll.h>
#include <readline.h>
#include <signal.h>
#include <unistd.h>

#include "client_state.h"
#include "cterm_ptr.h"
#include "exception_def.h"
#include "message.h"
#include "process_result.h"
#include "resource_holder.h"
#include "server_interaction_helper.h"
#include "signal_utils.h"
#include "socket_utils.h"
#include "string_utils.h"

// TODO (std_string) : think about config port number
#define PORT 22222
#define FD_COUNT 2
#define STDIN_INDEX 0
#define SOCKETD_INDEX 1

typedef std::function<execution_state(std::string const&)> request_handler_t;
typedef std::function<editor_state(message_response, client_state&)> response_handler_t;

// init
void initialize();
std::unordered_map<int, signal_handler_t> get_signal_handlers();
std::array<struct pollfd, FD_COUNT> create_fdarray(int socketd);
std::unordered_map<std::string, request_handler_t> get_local_request_handlers();
std::unordered_map<std::string, response_handler_t> get_response_handlers();
// cleanup
void clear_fdarray(std::array<struct pollfd, FD_COUNT> &fdarray);
void cleanup();
// readline
void readline_handler(char *raw_data);
char** completion_func(const char *text, int start, int end);
// signals
void signal_handler(int signo);
// message
execution_state process_request(std::string const &request);
process_result process_responses(message_responses_t const &responses, client_state &state);

// global variables
client_state cstate;

int main()
{
    initialize();
    setup_signal_handlers(get_signal_handlers());
    resource_holder<int> socket_holder(create_socket(), [](int socketd){ close(socketd); });
    int socketd = socket_holder.get();
    connect(socketd, PORT);
    std::string init_prompt = retrieve_current_state(socketd);
    cstate.prompt = init_prompt;
    cstate.socketd = socketd;
    cstate.ex_state = EX_CONTINUE;
    cstate.ed_state = ED_INPUT;
    std::array<struct pollfd, FD_COUNT> fdarray = create_fdarray(socketd);
    while(EX_CONTINUE == cstate.ex_state)
    {
        clear_fdarray(fdarray);
        int poll_result = poll(fdarray.data(), FD_COUNT, -1);
        if (-1 == poll_result)
        {
            if (EINTR != errno)
                throw poll_error();
            continue;
        }
        if (POLLIN == (fdarray[STDIN_INDEX].revents & POLLIN))
        {
            if (ED_INPUT == cstate.ed_state)
                rl_callback_read_char();
        }
        if (POLLERR == (fdarray[STDIN_INDEX].revents & POLLERR))
            throw poll_error();
        if (POLLIN == (fdarray[SOCKETD_INDEX].revents & POLLIN))
        {
            message_responses_t responses = receive_message_responses(socketd, create_signal_mask());
            process_result result = process_responses(responses, cstate);
            cstate.ex_state = result.ex_state;
            cstate.ed_state = result.ed_state;
        }
        if (POLLERR == (fdarray[SOCKETD_INDEX].revents & POLLERR))
            throw poll_error();
    }
    cleanup();
    return 0;
}

void initialize()
{    
    // erl runtime
    erl_init(NULL, 0);
    // readline
    rl_attempted_completion_over = 1;
    // completion
    rl_attempted_completion_function = completion_func;
    rl_sort_completion_matches = 0;
    rl_ignore_completion_duplicates = 0;
    // singnals
    rl_catch_signals = 0;
    rl_catch_sigwinch = 0;
    // absent in readline 6.2
    // rl_change_environment = 0;
    // readline history
    using_history();
    // set cleanup for uncaught exceptions
    std::set_terminate(cleanup);
}

std::unordered_map<int, signal_handler_t> get_signal_handlers()
{
    return {{SIGINT, signal_handler}, {SIGQUIT, signal_handler}, {SIGWINCH, signal_handler}, {SIGTSTP, signal_handler}};
}

std::array<struct pollfd, FD_COUNT> create_fdarray(int socketd)
{
    std::array<struct pollfd, FD_COUNT> fdarray;
    fdarray[STDIN_INDEX].fd = STDIN_FILENO;
    fdarray[STDIN_INDEX].events = POLLIN;
    fdarray[SOCKETD_INDEX].fd = socketd;
    fdarray[SOCKETD_INDEX].events = POLLIN;
    return fdarray;
}

std::unordered_map<std::string, request_handler_t> get_local_request_handlers()
{
    return {{"exit", [](std::string const &request){ return EX_FINISH; }}};
}

std::unordered_map<std::string, response_handler_t> get_response_handlers()
{
    return {
        {COMMAND_OUT, [](message_response response, client_state &state){ std::cout << response.data; return ED_COMMAND; }},
        {COMMAND_ERR, [](message_response response, client_state &state){ std::cerr << response.data; return ED_COMMAND; }},
        {COMMAND_END, [](message_response response, client_state &state){ state.prompt = response.data; return ED_INPUT; }}
    };
}

void clear_fdarray(std::array<struct pollfd, FD_COUNT> &fdarray)
{
    fdarray[STDIN_INDEX].revents = 0;
    fdarray[SOCKETD_INDEX].revents = 0;
}

void cleanup()
{
    rl_callback_handler_remove();
    rl_deprep_terminal();
}

void readline_handler(char *raw_data)
{
    if (nullptr == raw_data)
    {
        cstate.ex_state = EX_FINISH;
        rl_callback_handler_remove();
        return;
    }
    cterm_ptr<char> raw_data_ptr(raw_data);
    std::string line = trim_full(raw_data_ptr.get());
    if (line.empty())
        return;
    char *expansion = nullptr;
    int result = history_expand(const_cast<char*>(line.c_str()), &expansion);
    cterm_ptr<char> expansion_ptr(expansion);
    if (result == 0 || result == 1)
        add_history(expansion);
    std::string request(expansion_ptr.get());
    execution_state ex_state = process_request(request);
    rl_callback_handler_remove();
    setup_signal_handlers(get_signal_handlers());
    cstate.ex_state = ex_state;
    cstate.ed_state = ED_COMMAND;
}

char** completion_func(const char *text, int start, int end)
{
    std::string line = trim_left(text);
    std::vector<std::string> extensions = retrieve_extensions(cstate.socketd, line, create_signal_mask());
    // NULL terminated array
    size_t extensions_size = extensions.size();
    char** completion_array = (char**) malloc((extensions_size + 1) * sizeof(char*));
    for(size_t index = 0; index < extensions_size; ++index)
        completion_array[index] = duplicate_cstr(extensions.at(index));
    completion_array[extensions_size] = nullptr;
    return completion_array;
}

void handle_sigint()
{
    switch (cstate.ed_state)
    {
        case ED_INPUT:
            std::cout << "^C" << std::endl;
            rl_callback_handler_remove();
            rl_callback_handler_install(cstate.prompt.c_str(), readline_handler);
            setup_signal_handlers(get_signal_handlers());
            break;
        case ED_COMMAND:
            std::cout << std::endl;
            send_interrupt(cstate.socketd);
            break;
    }
}

void signal_handler(int signo)
{
    switch (signo)
    {
        case SIGINT:
            handle_sigint();
            break;
        case SIGQUIT:
            if (ED_INPUT == cstate.ed_state)
                std::cout << "^\\" << std::endl;
            cstate.ex_state = EX_FINISH;
            break;
        case SIGWINCH:
            break;
        case SIGTSTP:
            if (ED_INPUT == cstate.ed_state)
                std::cout << "^Z" << std::endl;
            cstate.ex_state = EX_FINISH;
            break;
    }
}

execution_state process_request(std::string const &request)
{
    std::unordered_map<std::string, request_handler_t> local_handlers = get_local_request_handlers();
    std::unordered_map<std::string, request_handler_t>::const_iterator iterator = local_handlers.find(request);
    if (local_handlers.end() != iterator)
        return iterator->second(request);
    send_command(cstate.socketd, request, create_signal_mask());
    return EX_CONTINUE;
}

process_result process_responses(message_responses_t const &responses, client_state &state)
{
    editor_state ed_state = ED_COMMAND;
    std::unordered_map<std::string, response_handler_t> response_handlers = get_response_handlers();
    for(message_response response : responses)
    {
        response_handler_t const &handler = response_handlers.at(response.type);
        editor_state handler_result = handler(response, state);
        if (ED_INPUT == handler_result)
            ed_state = ED_INPUT;
    }
    return process_result(EX_CONTINUE, ed_state);
}