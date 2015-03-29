#ifndef H_CONFIG_DEF
#define H_CONFIG_DEF

#include <string>

namespace cli_terminal
{

// config file location
const std::string config_file_location_key = "--config";
const std::string default_config_file_location = "/etc/cli_terminal/cli_terminal.conf";
// port number
const std::string port_number_key = "port_number";

}

#endif