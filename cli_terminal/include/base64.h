#ifndef H_BASE64
#define H_BASE64

#include <string>

namespace cli_terminal
{

std::string to_base64(std::string const& source);
std::string from_base64(std::string const& source);

}

#endif
