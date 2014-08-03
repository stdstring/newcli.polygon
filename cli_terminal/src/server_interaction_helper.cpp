#include <functional>
#include <string>
#include <vector>

#include <signal.h>

#include "exception_def.h"
#include "message.h"
#include "server_interaction.h"
#include "server_interaction_helper.h"
#include "signal_safe_executer.h"

std::string retrieve_current_state(int socketd, sigset_t mask)
{
    signal_safe_executer executer(mask);
    std::function<message_response()> func = [socketd](){ return sync_exchange<current_state_request, message_response>(socketd, current_state_request()); };
    message_response response = executer.execute<message_response>(func);
    /*if (0 != response.type.compare(CURRENT_STATE))
        throw bad_message();*/
    return response.data;
}

std::string retrieve_current_state(int socketd)
{
    message_response response = sync_exchange<current_state_request, message_response>(socketd, current_state_request());
    /*if (0 != response.type.compare(CURRENT_STATE))
        throw bad_message();*/
    return response.data;
}

message_responses_t receive_message_responses(int socketd, sigset_t mask)
{
    signal_safe_executer executer(mask);
    return executer.execute<message_responses_t>([socketd](){ return read_messages<message_response>(socketd); });
}

std::vector<std::string> retrieve_extensions(int socketd, std::string const &line, sigset_t mask)
{
    signal_safe_executer executer(mask);
    std::function<extension_response()> func = [socketd, &line](){ return sync_exchange<extension_request, extension_response>(socketd, extension_request(line)); };
    extension_response response = executer.execute<extension_response>(func);
    return response.extensions;
}

void send_interrupt(int socketd)
{
    write_message(socketd, interrupt_request());
}

void send_command(int socketd, std::string const &line, sigset_t mask)
{
    signal_safe_executer executer(mask);
    executer.execute([socketd, &line](){ write_message(socketd, command_request(line)); });
}