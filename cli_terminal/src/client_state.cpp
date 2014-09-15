#include <memory>

#include "client_state.h"
#include "iterminal_behavior.h"

namespace cli_terminal
{

void set_behavior(client_state &state, std::shared_ptr<iterminal_behavior> behavior)
{
    std::shared_ptr<iterminal_behavior> prev_behavior = state.get_behavior();
    if (prev_behavior)
        prev_behavior->clear_input_action();
    state.set_behavior(behavior);
    behavior->install_signal_action();
    behavior->install_input_action();
}

}