#ifndef H_CLI_IO_HELPER
#define H_CLI_IO_HELPER

#include <string>

#include "client_state.h"
#include "message.h"
#include "server_interaction_helper.h"

namespace cli_terminal
{

execution_state process_request(std::string const &request, client_state &state);

execution_state process_responses(message_responses_t const &responses, client_state &state);

}

#endif