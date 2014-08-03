#ifndef H_SERVER_INTERACTION_HELPER
#define H_SERVER_INTERACTION_HELPER

#include <signal.h>
#include <string>
#include <vector>

#include "message.h"

typedef std::vector<message_response> message_responses_t;

std::string retrieve_current_state(int socketd, sigset_t mask);

std::string retrieve_current_state(int socketd);

message_responses_t receive_message_responses(int socketd, sigset_t mask);

std::vector<std::string> retrieve_extensions(int socketd, std::string const &line, sigset_t mask);

void send_interrupt(int socketd);

void send_command(int socketd, std::string const &line, sigset_t mask);

#endif