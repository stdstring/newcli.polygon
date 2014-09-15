#include <functional>
#include <string>
#include <vector>
#include <signal.h>

#include "exception_def.h"
#include "message.h"
#include "server_interaction.h"
#include "server_interaction_helper.h"
#include "signal_safe_executer.h"

namespace cli_terminal
{

std::string retrieve_current_state(int socketd, sigset_t mask)
{
    signal_safe_executer executer(mask);
    std::function<current_state_response()> func = [socketd](){ return sync_exchange<current_state_request, current_state_response>(socketd, current_state_request()); };
    current_state_response response = executer.execute<current_state_response>(func);
    return response.prompt;
}

std::string retrieve_current_state(int socketd)
{
    current_state_response response = sync_exchange<current_state_request, current_state_response>(socketd, current_state_request());
    return response.prompt;
}

message_responses_t receive_message_responses(int socketd, sigset_t mask)
{
    signal_safe_executer executer(mask);
    return executer.execute<message_responses_t>([socketd](){ return read_messages<message_response>(socketd); });
}

std::vector<std::string> retrieve_extensions(int socketd, std::string const &line)
{
    extension_response response = sync_exchange<extension_request, extension_response>(socketd, extension_request(line));
    return response.extensions;
}

void interrupt_command(int socketd)
{
    write_message(socketd, interrupt_request());
}

void process_command(int socketd, std::string const &line)
{
    write_message(socketd, command_request(line));
}

void end_execution(int socketd, sigset_t mask)
{
    signal_safe_executer executer(mask);
    executer.execute([socketd](){ write_message(socketd, exit_request()); });
}

}