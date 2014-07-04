#include <algorithm>
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>
#include <vector>
// readline library
#include <readline.h>
#include <history.h>

void process_data(char* raw_data);
void init_readline();
char* duplicate_str(std::string const &source);
std::string trim_left(std::string const &source);
std::string trim_right(std::string const& source);
std::string trim_full(std::string const& source);

// completion functions
char** completion_func(const char *text, int start, int end);
char* generator_func(const char *text, int state);

std::vector<std::string> completion_data = {"iddqd666", "idkfa777", "idclip888", "iddqd999"};

int main()
{
    init_readline();
    std::cout << "readline usage example start" << std::endl;
    for(;;)
    {
        char *raw_data = readline("readline usage example >>>");
        if (!raw_data)
            break;
        process_data(raw_data);
        free(raw_data);
    }
    return 0;
}

void process_data(char* raw_data)
{
    std::string raw_str = std::string(raw_data);
    std::string line = trim_full(raw_str);
    std::cout << "line: " << line << " size: " << line.size() << std::endl;
    if (line.empty())
        return;
    char *expansion = nullptr;
    int result = history_expand(const_cast<char*>(line.c_str()), &expansion);
    if (result)
        std::cout << "expansion result: " << expansion <<  std::endl;
    if (result == 0 || result == 1)
        add_history(expansion);
    if (expansion != nullptr)
        free(expansion);
}

void init_readline()
{
    //rl_readline_name = "terminal_work_example_2";
    rl_attempted_completion_over = 1;
    rl_attempted_completion_function = completion_func;
    rl_sort_completion_matches = 0;
    rl_ignore_completion_duplicates = 0;
    using_history();
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

char* duplicate_str(std::string const &source)
{
    const char* source_str = source.c_str();
    char *buffer = (char*) malloc(strlen(source_str) + 1);
    strcpy(buffer, source_str);
    return buffer;
}

char** completion_func(const char *text, int start, int end)
{
    return rl_completion_matches(text, generator_func);
}

char* generator_func(const char *text, int state)
{
    static size_t index;
    if (state == 0)
        index = 0;
    if (index < completion_data.size())
        return duplicate_str(completion_data.at(index++));
    return (char*) NULL;
}