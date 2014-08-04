#include <algorithm>
#include <string>
#include <cstdlib>
#include <cstring>
#include "string_utils.h"

namespace cli_terminal
{

std::string trim_left(std::string const &source)
{
    std::string::const_iterator end = source.end();
    std::string::const_iterator new_begin = std::find_if(source.begin(), end, [](char c){return !std::isspace(c);});
    return new_begin != end ? std::string(new_begin, end) : std::string();
}

std::string trim_right(std::string const& source)
{
    std::string::const_reverse_iterator rend = source.rend();
    std::string::const_reverse_iterator new_rbegin = std::find_if(source.rbegin(), rend, [](char c){return !std::isspace(c);});
    return new_rbegin != rend ? std::string(source.begin(), new_rbegin.base()) : std::string();
}

std::string trim_full(std::string const& source)
{
    std::string trim_left_result = trim_left(source);
    return trim_right(trim_left_result);
}

char* duplicate_cstr(std::string const &source)
{
    size_t length = (source.size() + 1) * sizeof(char);
    char *buffer = (char*) malloc(length);
    memcpy(buffer, source.c_str(), length);
    return buffer;
}

}