#include <algorithm>
#include <string>
#include <vector>
#include "cli_terminal_config.h"
#include "config_reader.h"
#include "exception_def.h"

namespace cli_terminal
{

config_entry find_by_key(std::vector<config_entry> const &config, std::string const &key)
{
    typedef std::vector<config_entry>::const_iterator config_iterator_t;
    config_iterator_t result = std::find_if(config.begin(),
                                            config.end(),
                                            [&key](config_entry const &entry){ return entry.key == key; });
    if (config.end() == result)
        throw missing_config();
    return *result;
}

cli_terminal_config::cli_terminal_config(std::vector<config_entry> const &config) : _config(config)
{
    _port_number = std::stoi(find_by_key(config, port_number_key).value);
}

}
