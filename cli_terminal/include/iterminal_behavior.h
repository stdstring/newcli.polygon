#ifndef H_ITERMINAL_BEHAVIOR
#define H_ITERMINAL_BEHAVIOR

#include "execution_state.h"
#include "message.h"

namespace cli_terminal
{

class iterminal_behavior
{
public:
    virtual void clear_input_action() = 0;
    virtual void install_input_action() = 0;
    virtual void install_signal_action() = 0;
    virtual void process_char() = 0;
    virtual execution_state process_server_responses(message_responses_t const &responses) = 0;
};

}

#endif