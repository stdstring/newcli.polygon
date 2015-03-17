#ifndef H_CONFIG_READER
#define H_CONFIG_READER

#include <istream>
#include <string>
#include <vector>

namespace cli_terminal
{

class config_entry
{
public:
    config_entry() {}
    config_entry(std::string const &key_param, std::string const &value_param) : key(key_param), value(value_param) {}

    std::string key;
    std::string value;
};

std::vector<config_entry> read_config(std::istream source);

}

#endif