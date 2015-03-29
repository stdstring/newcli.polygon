#include <algorithm>
#include <functional>
#include <istream>
#include <string>
#include <utility>
#include <vector>

#include "config_reader.h"
#include "exception_def.h"
#include "string_utils.h"

namespace cli_terminal
{

const char key_value_delimiter = '=';
const char comment_header = '#';

typedef std::function<bool(std::string const&)> filter_fun_t;

std::vector<filter_fun_t> filters =
{
    [](std::string const &source){ return !source.empty(); },
    [](std::string const &source){ return (!source.empty()) && (source.front() != comment_header); }
};

bool filter(std::string const &source)
{
    for(filter_fun_t filter_fun : filters)
    {
        if (!filter_fun(source))
            return false;
    }
    return true;
}

typedef std::pair<bool, config_entry> parse_result_t;

parse_result_t parse(std::string const &source)
{
    typedef std::string::const_iterator string_iterator_t;
    string_iterator_t delimiter = std::find(source.begin(), source.end(), key_value_delimiter);
    if (source.end() == delimiter)
        return parse_result_t(false, config_entry());
    std::string key(source.begin(), delimiter);
    std::string value(delimiter + 1, source.end());
    config_entry entry(key, value);
    return parse_result_t(true, entry);
}

std::vector<config_entry> read_config(std::istream &source)
{
    std::vector<std::string> storage;
    while(!source.eof())
    {
        std::string raw_line;
        std::getline(source, raw_line);
        storage.push_back(raw_line);
    }
    return read_config(storage);
}

std::vector<config_entry> read_config(std::vector<std::string> const &source)
{
    std::vector<config_entry> storage;
    for(std::string raw_line : source)
    {
        std::string line = trim_full(raw_line);
        if (!filter(line))
            continue;
        parse_result_t parse_result = parse(line);
        if (!parse_result.first)
            // TODO (std_string) : probably throw some exception
            continue;
        storage.push_back(parse_result.second);
    }
    return storage;
}

typedef std::vector<config_entry>::const_iterator config_iterator_t;

std::string find_value(std::vector<config_entry> const &config, std::string const &key)
{
    config_iterator_t result = std::find_if(config.begin(),
                                            config.end(),
                                            [&key](config_entry const &entry){ return entry.key == key; });
    if (config.end() == result)
        throw missing_config();
    return result->value;
}

std::string find_value(std::vector<config_entry> const &config, std::string const &key, std::string const &default_value)
{
    config_iterator_t result = std::find_if(config.begin(),
                                            config.end(),
                                            [&key](config_entry const &entry){ return entry.key == key; });
    return (config.end() == result) ? default_value : result->value;
}

}