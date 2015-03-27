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

typedef std::vector<config_entry>::const_iterator config_iterator_t;

std::vector<config_entry> read_config(std::istream &source);
std::vector<config_entry> read_config(std::vector<std::string> const &source);

std::string find_value(std::vector<config_entry> const &config, std::string const &key);
std::string find_value(std::vector<config_entry> const &config, std::string const &key, std::string const &default_value);

}

#endif