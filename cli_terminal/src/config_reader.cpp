#include <algorithm>
#include <functional>
#include <istream>
#include <string>
#include <utility>
#include <vector>

#include "config_reader.h"
#include "string_utils.h"

#define KEY_VALUE_DELIMITER '='
#define COMMENT '#'

namespace cli_terminal
{

typedef std::function<bool(std::string const&)> filter_fun_t;
std::vector<filter_fun_t> filters =
{
    [](std::string const &source){ return !source.empty(); },
    [](std::string const &source){ return (!source.empty()) && (source.front() != COMMENT); }
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
    string_iterator_t delimiter = std::find(source.begin(), source.end(), KEY_VALUE_DELIMITER);
    if (source.end() == delimiter)
        return parse_result_t(false, config_entry());
    std::string key(source.begin(), delimiter);
    std::string value(delimiter + 1, source.end());
    config_entry entry(key, value);
    return parse_result_t(true, entry);
}

std::vector<config_entry> read_config(std::istream source)
{
    std::vector<config_entry> storage;
    while(!source.eof())
    {
        std::string raw_line;
        std::getline(source, raw_line);
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

}