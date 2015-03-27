#include <algorithm>
#include <array>
#include <exception>
#include <fstream>
#include <iterator>
#include <memory>
#include <string>
#include <vector>

#include <erl_interface.h>
#include <history.h>
#include <poll.h>
#include <readline.h>
#include <unistd.h>

#include "cli_io_helper.h"
#include "cli_terminal_config.h"
#include "client_state.h"
#include "config_def.h"
#include "config_reader.h"
#include "exception_def.h"
#include "fd_helper.h"
#include "input_terminal_behavior.h"
#include "iterminal_behavior.h"
#include "resource_holder.h"
#include "server_interaction_helper.h"
#include "signal_utils.h"
#include "socket_utils.h"

namespace cli_terminal
{

/*// TODO (std_string) : think about config port number
const int port_number = 6666;*/

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
    erl_init(NULL, 0);
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

cli_terminal_config read_config(int argc, char *argv[])
{
    // from http://publications.gbdirect.co.uk/c_book/chapter10/arguments_to_main.html:
    // When a program starts, the arguments to main will have been initialized to meet the following conditions:
    // argc is greater than zero.
    // argv[argc] is a null pointer.
    // argv[0] through to argv[argc-1] are pointers to strings whose meaning will be determined by the program.
    // argv[0] will be a string containing the program's name or a null string if that is not available.
    // Remaining elements of argv represent the arguments supplied to the program.
    // In cases where there is only support for single-case characters, the contents of these strings will be supplied to the program in lower-case.
    std::vector<std::string> args_source;
    std::copy(&argv[1], &argv[argc], std::back_inserter(args_source));
    std::vector<config_entry> args_entries = read_config(args_source);
    std::string config_file_location = find_value(args_entries, config_file_location_key, default_config_file_location);
    /*// remove config file location from args entries
    config_iterator_t config_file_pos = std::find_if(args_entries.begin(),
                                                     args_entries.end(),
                                                     [](config_entry const& entry){ return entry.key == config_file_location_key; });
    args_entries.erase(config_file_pos);*/
    std::fstream config_stream;
    config_stream.open(config_file_location, std::fstream::in);
    if (!config_stream.is_open())
        throw bad_config_file();
    std::vector<config_entry> config = read_config(config_stream);
    config_stream.close();
    std::copy(args_entries.begin(), args_entries.end(), std::back_inserter(config));
    return cli_terminal_config(config);
}

int main_impl(int argc, char *argv[])
{
    cli_terminal_config config = read_config(argc, argv);
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
            execution_state result = process_responses(responses, cstate);
            cstate.set_execution_state(result);
        }
        if (POLLERR == (fdarray[socketd_index].revents & POLLERR))
            throw poll_error();
    }
    cleanup();
    end_execution(socketd, create_signal_mask());
    return 0;
}

}

int main(int argc, char *argv[])
{
    return cli_terminal::main_impl(argc, argv);
}