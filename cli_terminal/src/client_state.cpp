#include <memory>

#include "client_state.h"
#include "iterminal_behavior.h"

namespace cli_terminal
{

void set_behavior(client_state &state, std::shared_ptr<iterminal_behavior> behavior)
{
    state.set_behavior(behavior);
    behavior->install_signal_action();
    behavior->install_input_action();
}

}