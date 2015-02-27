#ifndef H_HELP_PROCESSOR
#define H_HELP_PROCESSOR

#include <string>

namespace cli_terminal
{

std::string process_help_string(int socketd, std::string const &command_line);

}

#endif