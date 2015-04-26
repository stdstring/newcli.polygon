#ifndef H_CLI_TERMINAL_CONFIG
#define H_CLI_TERMINAL_CONFIG

#include <string>
#include <vector>
#include "config_reader.h"

namespace cli_terminal
{

class cli_terminal_config
{
public:
    cli_terminal_config(std::vector<config_entry> const &config);

    std::vector<config_entry> get_config() const { return _config; }
    int get_port_number() const { return _port_number; }
    /*int get_login_attempt_count() const { return _login_attempt_count; }*/

private:
    std::vector<config_entry> _config;
    int _port_number;
    /*int _login_attempt_count;*/
};

cli_terminal_config create_config(int argc, char *argv[]);

}

#endif