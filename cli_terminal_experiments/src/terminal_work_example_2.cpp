#include <algorithm>
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>
#include <vector>
// posix
#include <signal.h>
// readline library
#include <readline.h>
#include <history.h>

void process_data(char* raw_data);
void init_readline();
char* duplicate_str(std::string const &source);
std::string trim_left(std::string const &source);
std::string trim_right(std::string const& source);
std::string trim_full(std::string const& source);

// signal functions
void setup_signal_handlers();
void signal_handler(int signo);

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
        {
            std::cout << "empty raw_data" << std::endl;
            break;
        }
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
    // singnals
    rl_catch_signals = 0;
    rl_catch_sigwinch = 0;
    // absent in readline 6.2
    // rl_change_environment = 0;
    setup_signal_handlers();
    // history
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

void setup_signal_handlers()
{
    sigset_t oldset, newset;
    sigemptyset(&newset);
    sigaddset(&newset, SIGINT);
    sigaddset(&newset, SIGQUIT);
    sigaddset(&newset, SIGWINCH);
    sigaddset(&newset, SIGTSTP);
    sigprocmask(SIG_SETMASK, &newset, &oldset);
    sigset_t signal_mask;
    sigemptyset(&signal_mask);
    sigaddset(&signal_mask, SIGINT);
    sigaddset(&signal_mask, SIGQUIT);
    sigaddset(&signal_mask, SIGTERM);
    sigaddset(&signal_mask, SIGHUP);
    sigaddset(&signal_mask, SIGALRM);
    sigaddset(&signal_mask, SIGTSTP);
    sigaddset(&signal_mask, SIGTTIN);
    sigaddset(&signal_mask, SIGTTOU);
    sigaddset(&signal_mask, SIGWINCH);
    struct sigaction int_action;
    int_action.sa_handler = signal_handler;
    int_action.sa_mask = signal_mask;
    sigaction(SIGINT, &int_action, nullptr);
    struct sigaction quit_action;
    quit_action.sa_handler = signal_handler;
    quit_action.sa_mask = signal_mask;
    sigaction(SIGQUIT, &quit_action, nullptr);
    struct sigaction winch_action;
    winch_action.sa_handler = signal_handler;
    winch_action.sa_mask = signal_mask;
    sigaction(SIGWINCH, &winch_action, nullptr);
    struct sigaction tstp_action;
    tstp_action.sa_handler = signal_handler;
    tstp_action.sa_mask = signal_mask;
    sigaction(SIGTSTP, &tstp_action, nullptr);
    sigprocmask(SIG_SETMASK, &oldset, nullptr);
}

void signal_handler(int signo)
{
    if (signo == SIGINT)
    {
        //rl_free_line_state();
        //rl_cleanup_after_signal();
        //std::cout << "in SIGINT signal handler >>>" << std::endl;
        rl_free_line_state();
        rl_cleanup_after_signal();        
        rl_echo_signal_char(SIGINT);
        rl_on_new_line();
    }
    if (signo == SIGQUIT)
    {
        //rl_cleanup_after_signal();
        std::cout << "in SIGQUIT signal handler" << std::endl;
    }
    if (signo == SIGWINCH)
    {
        //rl_cleanup_after_signal();
        std::cout << "in SIGWINCH signal handler" << std::endl;
    }
    if (signo == SIGTSTP)
    {
        rl_cleanup_after_signal();
        std::cout << "in SIGTSTP signal handler" << std::endl;
        std::exit(0);
    }
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