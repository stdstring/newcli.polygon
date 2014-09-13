#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include <history.h>
#include <readline.h>
#include "signal.h"

#include "cli_io_helper.h"
#include "client_state.h"
#include "cterm_ptr.h"
#include "input_terminal_behavior.h"
#include "server_interaction_helper.h"
#include "signal_utils.h"
#include "string_utils.h"

namespace cli_terminal
{

extern client_state cstate;

namespace input_terminal_behavior_impl
{

void install_signal_handlers();
void clear_input_handler();
void install_input_handler();

void sigint_handler(int signo)
{
    // TODO (std_string) : may be use rl_echo_signal_char(int)
    std::cout << "^C" << std::endl;
    clear_input_handler();
    install_input_handler();
    install_signal_handlers();
}

void sigquit_handler(int signo)
{
    // TODO (std_string) : may be use rl_echo_signal_char(int)
    std::cout << "^\\" << std::endl;
    cstate.set_execution_state(EX_FINISH);
    clear_input_handler();
    install_signal_handlers();
}

void sigwinch_handler(int signo)
{
    // TODO (std_string) : may be some reaction
    clear_input_handler();
    install_input_handler();
    install_signal_handlers();
}

void sigtstp_handler(int signo)
{
    // TODO (std_string) : may be use rl_echo_signal_char(int)
    std::cout << "^Z" << std::endl;
    cstate.set_execution_state(EX_FINISH);
    clear_input_handler();
    install_signal_handlers();
}

void install_signal_handlers()
{
    std::unordered_map<int, signal_handler_t> handlers =
        {{SIGINT, sigint_handler}, {SIGQUIT, sigquit_handler}, {SIGWINCH, sigwinch_handler}, {SIGTSTP, sigtstp_handler}};
    setup_signal_handlers(handlers);
}

void input_handler(char *raw_data)
{
    if (nullptr == raw_data)
    {
        cstate.set_execution_state(EX_FINISH);
        clear_input_handler();
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
    execution_state ex_state = process_request(request, cstate);
    cstate.set_execution_state(ex_state);
}

char** completion_func(const char *text, int start, int end)
{
    std::string line = trim_full(text);
    std::vector<std::string> extensions = retrieve_extensions(cstate.get_socketd(), line, create_signal_mask());
    // NULL terminated array
    size_t extensions_size = extensions.size();
    char** completion_array = (char**) malloc((extensions_size + 1) * sizeof(char*));
    for(size_t index = 0; index < extensions_size; ++index)
        completion_array[index] = duplicate_cstr(extensions.at(index));
    completion_array[extensions_size] = nullptr;
    return completion_array;
}

void clear_input_handler()
{
    rl_callback_handler_remove();
}

void install_input_handler()
{
    // completion
    rl_attempted_completion_function = completion_func;
    rl_sort_completion_matches = 0;
    rl_ignore_completion_duplicates = 0;
    // readline handler
    std::string prompt = cstate.get_prompt();
    rl_callback_handler_install(prompt.c_str(), input_handler);
}

void process_char()
{
    rl_callback_read_char();
}

}

void input_terminal_behavior::install_input_action()
{
    input_terminal_behavior_impl::clear_input_handler();
    input_terminal_behavior_impl::install_input_handler();
}

void input_terminal_behavior::install_signal_action()
{
    input_terminal_behavior_impl::install_signal_handlers();
}

void input_terminal_behavior::process_char()
{
    input_terminal_behavior_impl::process_char();
}

}