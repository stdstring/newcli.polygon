#include <array>
#include <exception>
#include <memory>
#include <string>

#include <erl_interface.h>
#include <history.h>
#include <poll.h>
#include <readline.h>
#include <unistd.h>

#include "cli_io_helper.h"
#include "client_state.h"
#include "exception_def.h"
#include "fd_helper.h"
#include "input_terminal_behavior.h"
#include "iterminal_behavior.h"
#include "process_result.h"
#include "resource_holder.h"
#include "server_interaction_helper.h"
#include "signal_utils.h"
#include "socket_utils.h"

namespace cli_terminal
{

// TODO (std_string) : think about config port number
#define PORT 6666

// global variables
client_state cstate;

void cleanup()
{
    rl_callback_handler_remove();
    rl_deprep_terminal();
}

void initialize()
{    
    // erl runtime
    erl_init(NULL, 0);
    // readline
    rl_attempted_completion_over = 1;
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

int main_impl()
{
    initialize();
    resource_holder<int> socket_holder(create_socket(), [](int socketd){ close(socketd); });
    int socketd = socket_holder.get();
    connect(socketd, PORT);
    std::string init_prompt = retrieve_current_state(socketd);
    cstate.set_prompt(init_prompt);
    cstate.set_socketd(socketd);
    cstate.set_execution_state(EX_CONTINUE);
    std::shared_ptr<iterminal_behavior> init_behavior(new input_terminal_behavior());
    cstate.set_behavior(init_behavior);
    init_behavior->install_signal_action();
    init_behavior->install_input_action();
    std::array<struct pollfd, FD_COUNT> fdarray = create_fdarray(socketd);
    while(EX_CONTINUE == cstate.get_execution_state())
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
            cstate.get_behavior()->process_char();
        if (POLLERR == (fdarray[STDIN_INDEX].revents & POLLERR))
            throw poll_error();
        if (POLLIN == (fdarray[SOCKETD_INDEX].revents & POLLIN))
        {
            message_responses_t responses = receive_message_responses(socketd, create_signal_mask());
            execution_state result = process_responses(responses, cstate);
            cstate.set_execution_state(result);
        }
        if (POLLERR == (fdarray[SOCKETD_INDEX].revents & POLLERR))
            throw poll_error();
    }
    cleanup();
    end_execution(socketd, create_signal_mask());
    return 0;
}

}

int main()
{
    return cli_terminal::main_impl();
}