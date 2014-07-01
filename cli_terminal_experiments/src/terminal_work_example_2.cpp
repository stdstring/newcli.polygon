#include <algorithm>
#include <cctype>
#include <iostream>
#include <string>
// readline library
#include <readline.h>
#include <history.h>

void init_readline();
std::string trim_left(std::string &source);
//std::string trim_right(std::string const& source);
//std::string trim_full(std::string const& source);

int main()
{
    init_readline();
    std::cout << "readline usage example start" << std::endl;
    for(;;)
    {
        char *raw_line = readline("readline usage example >>>");
        if (!raw_line)
            break;
        std::string raw_str = std::string(raw_line);
        std::string line = trim_left(raw_str);
        std::cout << "result: " << line << std::endl;
        free(raw_line);
    }
    return 0;
}

void init_readline()
{	
}

std::string trim_left(std::string &source)
{
    std::string::iterator end = source.end();
    std::string::iterator new_begin = std::find_if(source.begin(), end, [](int c){return !std::isspace(c);});
    return new_begin != end ? std::string(new_begin, end) : std::string();
}

/*std::string trim_right(std::string const& source)
{
}

std::string trim_full(std::string const& source)
{
}*/