#include <array>
#include <exception>
#include <string>

#include <erl_interface.h>
#include <history.h>
#include <poll.h>
#include <readline.h>
#include <unistd.h>

#include "cli_io_helper.h"
#include "cli_terminal_config.h"
#include "client_state.h"
#include "exception_def.h"
#include "execution_state.h"
#include "fd_helper.h"
#include "input_terminal_behavior.h"
#include "iterminal_behavior.h"
#include "resource_holder.h"
#include "server_interaction_helper.h"
#include "signal_utils.h"
#include "socket_utils.h"

namespace cli_terminal
{

// global variables
client_state cstate;
std::terminate_handler default_terminate_handler;

void cleanup()
{
    rl_callback_handler_remove();
    rl_deprep_terminal();
}

void process_uncaught_exception()
{
    cleanup();
    default_terminate_handler();
}

char* empty_completion_func(const char *text, int state)
{
    return nullptr;
}

void initialize()
{    
    // erl runtime
    erl_init(nullptr, 0);
    // readline
    // completion
    rl_completion_entry_function = empty_completion_func;
    rl_attempted_completion_over = 1;
    rl_attempted_completion_function = nullptr;
    // singnals
    rl_catch_signals = 0;
    rl_catch_sigwinch = 0;
    // absent in readline 6.2
    // rl_change_environment = 0;
    // readline history
    using_history();
    // set cleanup for uncaught exceptions
    default_terminate_handler = std::set_terminate(process_uncaught_exception);
}

void execute_main_loop(int socketd, std::array<struct pollfd, fd_count> &fdarray)
{
    while(EX_CONTINUE == cstate.get_execution_state())
    {
        clear_fdarray(fdarray);
        int poll_result = poll(fdarray.data(), fd_count, -1);
        if (-1 == poll_result)
        {
            if (EINTR != errno)
                throw poll_error();
            continue;
        }
        if (POLLIN == (fdarray[stdin_index].revents & POLLIN))
            cstate.get_behavior()->process_char();
        if (POLLERR == (fdarray[stdin_index].revents & POLLERR))
            throw poll_error();
        if (POLLIN == (fdarray[socketd_index].revents & POLLIN))
        {
            message_responses_t responses = receive_message_responses(socketd, create_signal_mask());
            execution_state result = cstate.get_behavior()->process_server_responses(responses);
            cstate.set_execution_state(result);
        }
        if (POLLERR == (fdarray[socketd_index].revents & POLLERR))
            throw poll_error();
    }
}

int main_impl(int argc, char *argv[])
{
    cli_terminal_config config = create_config(argc, argv);
    initialize();
    resource_holder<int> volatile socket_holder(create_socket(), [](int socketd){ close(socketd); });
    int socketd = socket_holder.get();
    connect(socketd, config.get_port_number());
    std::string init_prompt = retrieve_current_state(socketd);
    cstate.set_prompt(init_prompt);
    cstate.set_socketd(socketd);
    cstate.set_execution_state(EX_CONTINUE);
    set_behavior(cstate, std::shared_ptr<iterminal_behavior>(new input_terminal_behavior()));
    std::array<struct pollfd, fd_count> fdarray = create_fdarray(socketd);
    execute_main_loop(socketd, fdarray);
    cleanup();
    end_execution(socketd, create_signal_mask());
    return 0;
}

}

int main(int argc, char *argv[])
{
    return cli_terminal::main_impl(argc, argv);
}