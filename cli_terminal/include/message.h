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

struct ExpansionRequest
{
public:
    ExpansionRequest(std::string const &command_line_value) : command_line(command_line_value) {}

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

class ExpansionResponse
{
public:
    ExpansionResponse(std::vector<std::string> const &expansions_value) : expansions(expansions_value) {}

    std::vector<std::string> expansions;
};

#endif