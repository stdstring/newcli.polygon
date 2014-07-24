#ifndef H_MESSAGE
#define H_MESSAGE

#include <string>
#include <vector>

// requests

struct CommandRequest
{
public:
    CommandRequest(std::string const &command_line_value) : command_line(command_line_value) {}

    std::string command_line;
};

struct InterruptRequest
{
};

struct CurrentStateRequest
{
};

struct ExtensionRequest
{
public:
    ExtensionRequest(std::string const &command_line_value) : command_line(command_line_value) {}

    std::string command_line;
};

struct ExitRequest
{
};

// responses

struct MessageResponse
{
public:
    MessageResponse(std::string const &type_value, std::string const &data_value) : type(type_value), data(data_value) {}

    // type in (command_out, command_err, end, current_state)
    std::string type;
    std::string data;
};

class ExtensionResponse
{
public:
    ExtensionResponse(std::vector<std::string> const &extension_value) : extensions(extension_value) {}

    std::vector<std::string> extensions;
};

#endif