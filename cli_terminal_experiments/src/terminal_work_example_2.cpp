#include <algorithm>
#include <cctype>
#include <iostream>
#include <string>
// readline library
#include <readline.h>
#include <history.h>

void init_readline();
std::string trim_left(std::string const &source);
std::string trim_right(std::string const& source);
std::string trim_full(std::string const& source);

int main()
{
    init_readline();
    std::cout << "readline usage example start" << std::endl;
    for(;;)
    {
        char *raw_data = readline("readline usage example >>>");
        if (!raw_data)
            break;
        std::string raw_str = std::string(raw_data);
        std::string line = trim_full(raw_str);
        std::cout << "line: " << line << " size: " << line.size() << std::endl;
        free(raw_data);
    }
    return 0;
}

void init_readline()
{	
}

std::string trim_left(std::string const &source)
{
    std::string::const_iterator end = source.end();
    std::string::const_iterator new_begin = std::find_if(source.begin(), end, [](int c){return !std::isspace(c);});
    return new_begin != end ? std::string(new_begin, end) : std::string();
}

std::string trim_right(std::string const& source)
{
    std::string::const_reverse_iterator rend = source.rend();
    std::string::const_reverse_iterator new_rbegin = std::find_if(source.rbegin(), rend, [](int c){return !std::isspace(c);});
    return new_rbegin != rend ? std::string(source.begin(), new_rbegin.base()) : std::string();
}

std::string trim_full(std::string const& source)
{
    std::string trim_left_result = trim_left(source);
    return trim_right(trim_left_result);
}