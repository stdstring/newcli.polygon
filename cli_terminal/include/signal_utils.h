#ifndef H_SIGNAL_UTILS
#define H_SIGNAL_UTILS

#include <unordered_map>
#include "signal.h"

namespace cli_terminal
{

typedef void (*signal_handler_t) (int);

sigset_t create_signal_mask();
void setup_signal_handlers(std::unordered_map<int, signal_handler_t> const &signal_handlers);

}

#endif