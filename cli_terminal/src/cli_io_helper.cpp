#include <functional>
#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>

#include "cli_io_helper.h"
#include "client_state.h"
#include "command_terminal_behavior.h"
#include "input_terminal_behavior.h"
#include "login_command_terminal_behavior.h"
#include "iterminal_behavior.h"
#include "message.h"
#include "server_interaction_helper.h"
#include "signal_utils.h"

namespace cli_terminal
{

typedef std::function<execution_state(std::string const&, client_state&)> request_handler_t;
typedef std::function<execution_state(message_response, client_state&)> response_handler_t;

std::unordered_map<std::string, request_handler_t> get_local_request_handlers()
{
    request_handler_t login_handler = [](std::string const &request, client_state &state)
        {
            set_behavior(state, std::shared_ptr<iterminal_behavior>(new login_command_terminal_behavior()));
            return EX_CONTINUE;
        };
    return {
        {"login", login_handler},
        {"exit", [](std::string const &request, client_state &state){ return EX_FINISH; }}
    };
}

std::unordered_map<std::string, response_handler_t> get_response_handlers()
{
    response_handler_t end_handler = [](message_response response, client_state &state)
        {
            state.set_prompt(response.data);
            set_behavior(state, std::shared_ptr<iterminal_behavior>(new input_terminal_behavior()));
            return EX_CONTINUE;
        };
    return {
        {COMMAND_OUT, [](message_response response, client_state &state){ std::cout << response.data; return EX_CONTINUE; }},
        {COMMAND_ERR, [](message_response response, client_state &state){ std::cerr << response.data; return EX_CONTINUE; }},
        {COMMAND_END, end_handler},
        {ERROR, [](message_response response, client_state &state){ std::cerr << response.data; return EX_CONTINUE; }}
    };
}

execution_state process_request(std::string const &request, client_state &state)
{
    std::unordered_map<std::string, request_handler_t> local_handlers = get_local_request_handlers();
    std::unordered_map<std::string, request_handler_t>::const_iterator iterator = local_handlers.find(request);
    if (local_handlers.end() != iterator)
        return iterator->second(request, state);
    process_command(state.get_socketd(), request, create_signal_mask());
    set_behavior(state, std::shared_ptr<iterminal_behavior>(new command_terminal_behavior()));
    return EX_CONTINUE;
}

execution_state process_responses(message_responses_t const &responses, client_state &state)
{
    execution_state ex_state = EX_CONTINUE;
    std::unordered_map<std::string, response_handler_t> response_handlers = get_response_handlers();
    for(message_response response : responses)
    {
        response_handler_t const &handler = response_handlers.at(response.type);
        execution_state handler_result = handler(response, state);
        if (EX_FINISH == handler_result)
            ex_state = EX_FINISH;
    }
    return ex_state;
}

}