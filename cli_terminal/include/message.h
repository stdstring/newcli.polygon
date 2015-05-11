#ifndef H_MESSAGE
#define H_MESSAGE

#include <string>
#include <vector>

namespace cli_terminal
{

const std::string command_start_tag = "command";
const std::string command_out_tag = "command_out";
const std::string command_err_tag = "command_err";
const std::string command_end_tag = "end";
const std::string command_stop_tag = "stop";
const std::string error_tag = "error";
const std::string command_int_tag = "interrupt";
const std::string current_state_request_tag = "current_state_request";
const std::string current_state_response_tag = "current_state_response";
const std::string extension_request_tag = "extension_request";
const std::string extension_response_tag = "extension_response";
const std::string exit_tag = "exit";
const std::string help_request_tag = "help_request";
const std::string help_response_tag = "help_response";
const std::string suitable_commands_request_tag = "suitable_commands_request";
const std::string suitable_commands_response_tag = "suitable_commands_response";
const std::string login_request_tag = "login";
const std::string login_success_response_tag = "login_success";
const std::string login_fail_response_tag = "login_fail";
const std::string login_error_response_tag = "login_error";
const std::string mode_exit_request_tag = "current_mode_exit";
const std::string mode_exit_response_tag = "current_mode_exit";
const std::string mode_stop_response_tag = "exit";

// requests

struct command_request
{
public:
    command_request(std::string const &command_line_value) : command_line(command_line_value)
    {}

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
    extension_request(std::string const &command_line_value) : command_line(command_line_value)
    {}

    std::string command_line;
};

struct exit_request
{
};

struct help_request
{
public:
    help_request(std::string const &command_line_value) : command_line(command_line_value)
    {}

    std::string command_line;
};

struct suitable_commands_request
{
public:
    suitable_commands_request(std::string const &command_line_value) : command_line(command_line_value)
    {}

    std::string command_line;
};

struct mode_exit_request
{
};

struct login_request
{
public:
    login_request(std::string const &username_value, std::string const &password_value)
        : username(username_value), password(password_value)
    {}

    std::string username;
    std::string password;
};

// responses

struct message_response
{
public:
    message_response(std::string const &type_value, std::string const &data_value) : type(type_value), data(data_value)
    {}

    // type in (command_out, command_err, end, stop)
    std::string type;
    std::string data;
};

typedef std::vector<message_response> message_responses_t;

struct current_state_response
{
public:
    current_state_response(std::string const &prompt_value) : prompt(prompt_value)
    {}

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
    help_response(std::string const &help_value) : help(help_value)
    {}

    std::string help;
};

struct suitable_commands_response
{
public:
    suitable_commands_response(std::vector<std::string> const &commands_value) : commands(commands_value)
    {}

    std::vector<std::string> commands;
};

struct login_response
{
public:
    login_response(std::string const &type_value, std::string const &data_value)
        : type(type_value), data(data_value)
    {}

    // type in (login_success, login_fail, login_error)
    std::string type;
    std::string data;
};

struct mode_exit_response
{
public:
    mode_exit_response(std::string const &type_value, std::string const &data_value) : type(type_value), data(data_value)
    {}

    // type in (current_mode_exit, exit)
    std::string type;
    std::string data;
};

}

#endif