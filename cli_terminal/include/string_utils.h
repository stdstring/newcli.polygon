#ifndef H_STRING_UTILS
#define H_STRING_UTILS

#include <string>

namespace cli_terminal
{

std::string trim_left(std::string const &source);
std::string trim_right(std::string const& source);
std::string trim_full(std::string const& source);
char* duplicate_cstr(std::string const &source);

}

#endif