#include <algorithm>
#include <fstream>
#include <iterator>
#include <stdexcept>
#include <string>
#include <vector>

#include "cli_terminal_config.h"
#include "config_def.h"
#include "config_reader.h"
#include "exception_def.h"

namespace cli_terminal
{

const int port_number_min = 0;
const int port_number_max = 65535;
/*const int login_attempt_count_min = 0;*/

cli_terminal_config::cli_terminal_config(std::vector<config_entry> const &config) : _config(config)
{
    int port_number = std::stoi(find_value(config, port_number_key));
    if ((port_number < port_number_min) || (port_number > port_number_max))
        throw std::out_of_range(port_number_key);
    _port_number = port_number;
    /*int login_attempt_count = std::stoi(find_value(config, login_attempt_count_key));
    if (login_attempt_count < login_attempt_count_min)
        throw std::out_of_range(login_attempt_count_key);
    _login_attempt_count = login_attempt_count;*/
}

cli_terminal_config create_config(int argc, char *argv[])
{
    // from http://publications.gbdirect.co.uk/c_book/chapter10/arguments_to_main.html:
    // When a program starts, the arguments to main will have been initialized to meet the following conditions:
    // argc is greater than zero.
    // argv[argc] is a null pointer.
    // argv[0] through to argv[argc-1] are pointers to strings whose meaning will be determined by the program.
    // argv[0] will be a string containing the program's name or a null string if that is not available.
    // Remaining elements of argv represent the arguments supplied to the program.
    // In cases where there is only support for single-case characters, the contents of these strings will be supplied to the program in lower-case.
    std::vector<std::string> args_source;
    std::copy(&argv[1], &argv[argc], std::back_inserter(args_source));
    std::vector<config_entry> args_entries = read_config(args_source);
    std::string config_file_location = find_value(args_entries, config_file_location_key, default_config_file_location);
    std::fstream config_stream;
    config_stream.open(config_file_location, std::fstream::in);
    if (!config_stream.is_open())
        throw bad_config_file();
    std::vector<config_entry> config = read_config(config_stream);
    config_stream.close();
    std::copy(args_entries.begin(), args_entries.end(), std::back_inserter(config));
    return cli_terminal_config(config);
}

}
