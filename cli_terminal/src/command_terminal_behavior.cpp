#include <iostream>
#include <unordered_map>

#include <readline.h>
#include "signal.h"

#include "client_state.h"
#include "command_terminal_behavior.h"
#include "execution_state.h"
#include "message.h"
#include "server_interaction_helper.h"
#include "signal_utils.h"

namespace cli_terminal
{

extern client_state cstate;

namespace command_terminal_behavior_impl
{

void install_signal_handlers();

void sigint_handler(int signo)
{
    // ^C already is shown on screen
    std::cout << std::endl;
    interrupt_command(cstate.get_socketd());
    install_signal_handlers();
}

void sigquit_handler(int signo)
{
    // TODO (std_string) : may be use rl_echo_signal_char(int)
    std::cout << "^\\" << std::endl;
    cstate.set_execution_state(EX_FINISH);
    install_signal_handlers();
}

void sigwinch_handler(int signo)
{
    // TODO (std_string) : may be some reaction
    install_signal_handlers();
}

void sigtstp_handler(int signo)
{
    // TODO (std_string) : may be use rl_echo_signal_char(int)
    std::cout << "^Z" << std::endl;
    cstate.set_execution_state(EX_FINISH);
    install_signal_handlers();
}

void install_signal_handlers()
{
    std::unordered_map<int, signal_handler_t> handlers =
        {{SIGINT, sigint_handler}, {SIGQUIT, sigquit_handler}, {SIGWINCH, sigwinch_handler}, {SIGTSTP, sigtstp_handler}};
    setup_signal_handlers(handlers);
}

}

void command_terminal_behavior::clear_input_action()
{
    // do nothing now
}

void command_terminal_behavior::install_input_action()
{
    // do nothing now
}

void command_terminal_behavior::install_signal_action()
{
    command_terminal_behavior_impl::install_signal_handlers();
}

void command_terminal_behavior::process_char()
{
    // do nothing now
}

execution_state command_terminal_behavior::process_server_responses(message_responses_t const &responses)
{
    return EX_CONTINUE;
}

}