#include <array>
#include <exception>
#include <unordered_map>
#include <vector>
#include <erl_interface.h>
#include <history.h>
#include <poll.h>
#include <readline.h>
#include <signal.h>
#include <unistd.h>

#include "client_state.h"
#include "exception_def.h"
#include "message.h"
#include "process_result.h"
#include "resource_holder.h"
#include "server_interaction.h"
#include "signal_safe_executer.h"
#include "signal_utils.h"
#include "socket_utils.h"

// TODO (std_string) : think about config port number
#define PORT 22222
#define FD_COUNT 2
#define STDIN_INDEX 0
#define SOCKETD_INDEX 1

typedef std::vector<MessageResponse> MessageResponseVector;
typedef std::vector<MessageResponse>::const_iterator MessageResponseIterator;

// init
void initialize();
std::unordered_map<int, signal_handler_t> get_signal_handlers();
std::array<struct pollfd, FD_COUNT> create_fdarray(int socketd);
// cleanup
void clear_fdarray(std::array<struct pollfd, FD_COUNT> &fdarray);
void cleanup();
// readline
void readline_handler(char *raw_data);
char** completion_func(const char *text, int start, int end);
// signals
void signal_handler(int signo);
// message
ProcessResult process_responses(MessageResponseVector const &responses);

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
            ProcessResult result = process_responses(responses);
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
{}

char** completion_func(const char *text, int start, int end)
{}

void signal_handler(int signo)
{}

ProcessResult process_responses(MessageResponseVector const &responses)
{
    ExecutionState execution_state = EX_CONTINUE;
    EditorState editor_state = ED_COMMAND;
    MessageResponseIterator end = responses.end();
    for(MessageResponseIterator iterator = responses.begin(); iterator != end; ++iterator)
    {}
    return ProcessResult(execution_state, editor_state);
}