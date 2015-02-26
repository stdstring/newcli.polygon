#ifndef H_MESSAGE
#define H_MESSAGE

#include <string>
#include <vector>

#define COMMAND_START "command"
#define COMMAND_OUT "command_out"
#define COMMAND_ERR "command_err"
#define COMMAND_END "end"
#define ERROR "error"
#define COMMAND_STOP "interrupt"
#define CURRENT_STATE_REQUEST "current_state_request"
#define CURRENT_STATE_RESPONSE "current_state_response"
#define EXTENSION_REQUEST "extension_request"
#define EXTENSION_RESPONSE "extension_response"
#define EXIT "exit"
#define HELP_REQUEST "help_request"
#define HELP_RESPONSE "help_response"
#define SUITABLE_COMMANDS_REQUEST "suitable_commands_request"
#define SUITABLE_COMMANDS_RESPONSE "suitable_commands_response"
#define CURRENT_MODE_EXIT "current_mode_exit"

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

struct help_request
{
public:
    help_request(std::string const &command_line_value) : command_line(command_line_value) {}

    std::string command_line;
};

struct suitable_commands_request
{
public:
    suitable_commands_request(std::string const &command_line_value) : command_line(command_line_value) {}

    std::string command_line;
};

struct mode_exit_request
{
};

// responses

struct message_response
{
public:
    message_response(std::string const &type_value, std::string const &data_value) : type(type_value), data(data_value) {}

    // type in (command_out, command_err, end, current_state, help)
    std::string type;
    std::string data;
};

struct current_state_response
{
public:
    current_state_response(std::string const &prompt_value) : prompt(prompt_value) {}

    std::string prompt;
};

struct extension_response
{
public:
    extension_response(std::string const &common_prefix_value, std::vector<std::string> const &extension_value)
        : common_prefix(common_prefix_value), extensions(extension_value)
    {}

    std::string common_prefix;
    std::vector<std::string> extensions;
};

struct help_response
{
public:
    help_response(std::string const &help_value) : help(help_value) {}

    std::string help;
};

struct suitable_commands_response
{
public:
    suitable_commands_response(std::vector<std::string> const &commands_value) : commands(commands_value) {}

    std::vector<std::string> commands;
};

}

#endif