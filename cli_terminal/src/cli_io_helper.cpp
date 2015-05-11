#include <functional>
#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>

#include "cli_io_helper.h"
#include "client_state.h"
#include "command_terminal_behavior.h"
#include "empty_terminal_behavior.h"
#include "execution_state.h"
#include "input_terminal_behavior.h"
#include "iterminal_behavior.h"
#include "message.h"
#include "server_interaction_helper.h"

namespace cli_terminal
{

typedef std::function<execution_state(std::string const&, client_state&)> request_handler_t;
typedef std::function<execution_state(mode_exit_response const&, client_state&)> mode_exit_response_handler_t;

// TODO (std_string) : think about this
std::unordered_map<std::string, request_handler_t> get_local_request_handlers()
{
    return {};
}

execution_state process_request(std::string const &request, client_state &state)
{
    std::unordered_map<std::string, request_handler_t> local_handlers = get_local_request_handlers();
    std::unordered_map<std::string, request_handler_t>::const_iterator iterator = local_handlers.find(request);
    if (local_handlers.end() != iterator)
        return iterator->second(request, state);
    process_command(state.get_socketd(), request);
    set_behavior(state, std::shared_ptr<iterminal_behavior>(new command_terminal_behavior()));
    return EX_CONTINUE;
}

std::unordered_map<std::string, mode_exit_response_handler_t> get_mode_exit_response_handlers()
{
    mode_exit_response_handler_t mode_exit_response_handler = [](mode_exit_response const &response, client_state &state){
        state.set_prompt(response.data);
        set_behavior(state, std::shared_ptr<iterminal_behavior>(new input_terminal_behavior()));
        return EX_CONTINUE;
    };
    mode_exit_response_handler_t mode_stop_response_handler = [](mode_exit_response const &response, client_state &state){
        set_behavior(state, std::shared_ptr<iterminal_behavior>(new empty_terminal_behavior()));
        return EX_FINISH;
    };
    return {
        {mode_exit_response_tag, mode_exit_response_handler},
        {mode_stop_response_tag, mode_stop_response_handler}
    };
}

execution_state process_mode_exit(client_state &state)
{
    mode_exit_response response = current_mode_exit(state.get_socketd());
    std::unordered_map<std::string, mode_exit_response_handler_t> handlers = get_mode_exit_response_handlers();
    mode_exit_response_handler_t handler = handlers.at(response.type);
    return handler(response, state);
}

execution_state process_responses(message_responses_t const &responses,
                                  client_state &state,
                                  response_handlers_def_t const& response_handlers,
                                  response_handler_t default_handler)
{
    execution_state ex_state = EX_CONTINUE;
    for(message_response response : responses)
    {
        response_handlers_def_t::const_iterator handler = response_handlers.find(response.type);
        execution_state handler_result = response_handlers.end() == handler ?
                                         default_handler(response, state) :
                                         handler->second(response, state);
        if (EX_FINISH == handler_result)
            ex_state = EX_FINISH;
    }
    return ex_state;
}

execution_state skip_response(message_response const &response, client_state &state)
{
    return EX_CONTINUE;
}

}