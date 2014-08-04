#ifndef H_MESSAGE
#define H_MESSAGE

#include <string>
#include <vector>

#define COMMAND_START "command"
#define COMMAND_OUT "command_out"
#define COMMAND_ERR "command_err"
#define COMMAND_END "end"
#define COMMAND_STOP "interrupt"
#define CURRENT_STATE "current_state"
#define EXTENSION "extension"
#define EXIT "exit"

namespace cli_terminal
{

// requests

struct command_request
{
public:
    command_request(std::string const &command_line_value) : command_line(command_line_value) {}

    std::string command_line;
};

struct interrupt_request
{
};

struct current_state_request
{
};

struct extension_request
{
public:
    extension_request(std::string const &command_line_value) : command_line(command_line_value) {}

    std::string command_line;
};

struct exit_request
{
};

// responses

struct message_response
{
public:
    message_response(std::string const &type_value, std::string const &data_value) : type(type_value), data(data_value) {}

    // type in (command_out, command_err, end, current_state)
    std::string type;
    std::string data;
};

class extension_response
{
public:
    extension_response(std::vector<std::string> const &extension_value) : extensions(extension_value) {}

    std::vector<std::string> extensions;
};

}

#endif