#ifndef H_SERVER_INTERACTION_HELPER
#define H_SERVER_INTERACTION_HELPER

#include <signal.h>
#include <string>
#include <vector>

#include "message.h"

namespace cli_terminal
{

std::string retrieve_current_state(int socketd, sigset_t mask);

std::string retrieve_current_state(int socketd);

message_responses_t receive_message_responses(int socketd, sigset_t mask);

extension_response retrieve_extensions(int socketd, std::string const &line);

void interrupt_command(int socketd);

void process_command(int socketd, std::string const &line);

void end_execution(int socketd, sigset_t mask);

std::string retrieve_help(int socketd, std::string const &line);

std::vector<std::string> retrieve_suitable_commands(int socketd, std::string const &line);

mode_exit_response current_mode_exit(int socketd);

login_response login(int socketd, std::string const &username, std::string const &password);

}

#endif