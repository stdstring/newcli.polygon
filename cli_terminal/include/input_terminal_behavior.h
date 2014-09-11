#ifndef H_INPUT_TERMINAL_BEHAVIOR
#define H_INPUT_TERMINAL_BEHAVIOR

#include "iterminal_behavior.h"

namespace cli_terminal
{

class input_terminal_behavior : public iterminal_behavior
{
public:
    void install_input_action() override;
    void install_signal_action() override;
    void process_char() override;
};

}

#endif