#ifndef H_ITERMINAL_BEHAVIOR
#define H_ITERMINAL_BEHAVIOR

namespace cli_terminal
{

class iterminal_behavior
{
public:
    virtual void install_input_action() = 0;
    virtual void install_signal_action() = 0;
    virtual void process_char() = 0;
};

}

#endif