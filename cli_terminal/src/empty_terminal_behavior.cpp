#include "empty_terminal_behavior.h"
#include "execution_state.h"
#include "message.h"

namespace cli_terminal
{

void empty_terminal_behavior::clear_input_action()
{
    // do nothing now
}

void empty_terminal_behavior::install_input_action()
{
    // do nothing now
}

void empty_terminal_behavior::install_signal_action()
{
    // do nothing now
}

void empty_terminal_behavior::process_char()
{
    // do nothing now
}

execution_state empty_terminal_behavior::process_server_responses(message_responses_t const &responses)
{
    return EX_CONTINUE;
}

}