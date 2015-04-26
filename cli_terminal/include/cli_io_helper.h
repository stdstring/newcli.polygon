#ifndef H_CLI_IO_HELPER
#define H_CLI_IO_HELPER

#include <functional>
#include <string>
#include <unordered_map>

#include "client_state.h"
#include "execution_state.h"
#include "message.h"

namespace cli_terminal
{

typedef std::function<execution_state(message_response const&, client_state&)> response_handler_t;
typedef std::unordered_map<std::string, response_handler_t> response_handlers_def_t;

execution_state process_request(std::string const &request, client_state &state);

execution_state process_mode_exit(client_state &state);

execution_state process_responses(message_responses_t const &responses,
                                  client_state &state,
                                  response_handlers_def_t const& response_handlers,
                                  response_handler_t default_handler);

execution_state skip_response(message_response const &response, client_state &state);

}

#endif