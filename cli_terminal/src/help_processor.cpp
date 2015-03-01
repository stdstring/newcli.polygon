#include <locale>
#include <string>
#include <vector>

#include "help_processor.h"
#include "server_interaction_helper.h"
#include "string_utils.h"

namespace cli_terminal
{

std::string process_help(int socketd, std::string const &command_line)
{
    return retrieve_help(socketd, trim_full(command_line));
}

std::string process_suitable_commands(int socketd, std::string const &command_line)
{
    std::vector<std::string> commands = retrieve_suitable_commands(socketd, trim_left(command_line));
    return join(commands, "\t");
}

std::string process_help_string(int socketd, std::string const &command_line)
{
    return std::isspace(command_line.back()) ?
           process_help(socketd, command_line) :
           process_suitable_commands(socketd, command_line);
}

}