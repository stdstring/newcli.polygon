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
// login attempt count
const std::string login_attempt_count_key = "login_attempt_count";

}

#endif