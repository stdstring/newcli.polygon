#ifndef H_CLI_IO_HELPER
#define H_CLI_IO_HELPER

#include <string>

#include "client_state.h"
#include "message.h"
#include "process_result.h"
#include "server_interaction_helper.h"

execution_state process_request(std::string const &request, int socketd);

process_result process_responses(message_responses_t const &responses, client_state &state);

#endif