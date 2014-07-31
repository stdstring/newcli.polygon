#include <array>
#include <exception>
#include <functional>
#include <iostream>
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
#include "server_interaction.h"
#include "signal_safe_executer.h"
#include "signal_utils.h"
#include "socket_utils.h"
#include "string_utils.h"

// TODO (std_string) : think about config port number
#define PORT 22222
#define FD_COUNT 2
#define STDIN_INDEX 0
#define SOCKETD_INDEX 1

typedef std::vector<MessageResponse> MessageResponseVector;
typedef std::function<ExecutionState(std::string const&)> RequestHandler;
typedef std::function<EditorState(MessageResponse, ClientState&)> ResponseHandler;

// init
void initialize();
std::unordered_map<int, signal_handler_t> get_signal_handlers();
std::array<struct pollfd, FD_COUNT> create_fdarray(int socketd);
std::unordered_map<std::string, RequestHandler> get_local_request_handlers();
std::unordered_map<std::string, ResponseHandler> get_response_handlers();
// cleanup
void clear_fdarray(std::array<struct pollfd, FD_COUNT> &fdarray);
void cleanup();
// readline
void readline_handler(char *raw_data);
char** completion_func(const char *text, int start, int end);
// signals
void signal_handler(int signo);
// message
ExecutionState process_request(std::string const &request);
ProcessResult process_responses(MessageResponseVector const &responses, ClientState &state);

// global variables
ClientState client_state;

int main()
{
    initialize();
    setup_signal_handlers(get_signal_handlers());
    ResourceHolder<int> socket_holder(create_socket(), [](int socketd){ close(socketd); });
    int socketd = socket_holder.get();
    connect(socketd, PORT);
    MessageResponse init_state_response = sync_exchange<CurrentStateRequest, MessageResponse>(socketd, CurrentStateRequest());
    /*if (0 != init_state_response.type.compare(CURRENT_STATE))
        throw bad_message();*/
    client_state.prompt = init_state_response.data;
    client_state.socketd = socketd;
    client_state.execution_state = EX_CONTINUE;
    client_state.editor_state = ED_INPUT;
    SignalSafeExecuter executer(create_signal_mask());
    std::array<struct pollfd, FD_COUNT> fdarray = create_fdarray(socketd);
    while(EX_CONTINUE == client_state.execution_state)
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
            if (ED_INPUT == client_state.editor_state)
                rl_callback_read_char();
        }
        if (POLLERR == (fdarray[STDIN_INDEX].revents & POLLERR))
            throw poll_error();
        if (POLLIN == (fdarray[SOCKETD_INDEX].revents & POLLIN))
        {
            MessageResponseVector responses = executer.execute<MessageResponseVector>([&socketd](){ return read_messages<MessageResponse>(socketd); });
            ProcessResult result = process_responses(responses, client_state);
            client_state.execution_state = result.execution_state;
            client_state.editor_state = result.editor_state;
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

std::unordered_map<std::string, RequestHandler> get_local_request_handlers()
{
    return {{"exit", [](std::string const &request){ return EX_FINISH; }}};
}

std::unordered_map<std::string, ResponseHandler> get_response_handlers()
{
    return {
        {COMMAND_OUT, [](MessageResponse response, ClientState &state){ std::cout << response.data; return ED_COMMAND; }},
        {COMMAND_ERR, [](MessageResponse response, ClientState &state){ std::cerr << response.data; return ED_COMMAND; }},
        {COMMAND_END, [](MessageResponse response, ClientState &state){ state.prompt = response.data; return ED_INPUT; }}
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
        client_state.execution_state = EX_FINISH;
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
    ExecutionState execution_state = process_request(request);
    rl_callback_handler_remove();
    setup_signal_handlers(get_signal_handlers());
    client_state.execution_state = execution_state;
    client_state.editor_state = ED_COMMAND;
}

char** completion_func(const char *text, int start, int end)
{
    std::string line = trim_left(text);
    sigset_t mask = create_signal_mask();
    SignalSafeExecuter executer(mask);
    ExtensionResponse response = sync_exchange<ExtensionRequest, ExtensionResponse>(client_state.socketd, ExtensionRequest(line));
    std::vector<std::string> extensions = response.extensions;
    // NULL terminated array
    char** completion_array = (char**) malloc((extensions.size() + 1) * sizeof(char*));
    for(size_t index = 0; index < extensions.size(); ++index)
    {}
    completion_array[extensions.size()] = nullptr;
    return completion_array;
}

void signal_handler(int signo)
{}

ExecutionState process_request(std::string const &request)
{
    std::unordered_map<std::string, RequestHandler> local_handlers = get_local_request_handlers();
    std::unordered_map<std::string, RequestHandler>::const_iterator iterator = local_handlers.find(request);
    if (local_handlers.end() != iterator)
        return iterator->second(request);
    sigset_t mask = create_signal_mask();
    SignalSafeExecuter executer(mask);
    executer.execute([&request](){ write_message(client_state.socketd, CommandRequest(request)); });
    return EX_CONTINUE;
}

ProcessResult process_responses(MessageResponseVector const &responses, ClientState &state)
{
    EditorState editor_state = ED_COMMAND;
    std::unordered_map<std::string, ResponseHandler> response_handlers = get_response_handlers();
    for(MessageResponse response : responses)
    {
        ResponseHandler const &handler = response_handlers.at(response.type);
        EditorState handler_result = handler(response, state);
        if (ED_INPUT == handler_result)
            editor_state = ED_INPUT;
    }
    return ProcessResult(EX_CONTINUE, editor_state);
}