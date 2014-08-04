#include <functional>
#include <iostream>
#include <string>
#include <unordered_map>

#include "cli_io_helper.h"
#include "client_state.h"
#include "message.h"
#include "process_result.h"
#include "server_interaction_helper.h"
#include "signal_utils.h"

namespace cli_terminal
{

typedef std::function<execution_state(std::string const&)> request_handler_t;
typedef std::function<editor_state(message_response, client_state&)> response_handler_t;

std::unordered_map<std::string, request_handler_t> get_local_request_handlers()
{
    return {{"exit", [](std::string const &request){ return EX_FINISH; }}};
}

std::unordered_map<std::string, response_handler_t> get_response_handlers()
{
    return {
        {COMMAND_OUT, [](message_response response, client_state &state){ std::cout << response.data; return ED_COMMAND; }},
        {COMMAND_ERR, [](message_response response, client_state &state){ std::cerr << response.data; return ED_COMMAND; }},
        {COMMAND_END, [](message_response response, client_state &state){ state.prompt = response.data; return ED_INPUT; }}
    };
}

execution_state process_request(std::string const &request, int socketd)
{
    std::unordered_map<std::string, request_handler_t> local_handlers = get_local_request_handlers();
    std::unordered_map<std::string, request_handler_t>::const_iterator iterator = local_handlers.find(request);
    if (local_handlers.end() != iterator)
        return iterator->second(request);
    process_command(socketd, request, create_signal_mask());
    return EX_CONTINUE;
}

process_result process_responses(message_responses_t const &responses, client_state &state)
{
    editor_state ed_state = ED_COMMAND;
    std::unordered_map<std::string, response_handler_t> response_handlers = get_response_handlers();
    for(message_response response : responses)
    {
        response_handler_t const &handler = response_handlers.at(response.type);
        editor_state handler_result = handler(response, state);
        if (ED_INPUT == handler_result)
            ed_state = ED_INPUT;
    }
    return process_result(EX_CONTINUE, ed_state);
}

}