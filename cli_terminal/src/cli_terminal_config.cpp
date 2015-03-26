#include <algorithm>
#include <string>
#include <vector>

#include "cli_terminal_config.h"
#include "config_def.h"
#include "config_reader.h"

namespace cli_terminal
{

cli_terminal_config::cli_terminal_config(std::vector<config_entry> const &config) : _config(config)
{
    _port_number = std::stoi(find_value(config, port_number_key));
}

}
