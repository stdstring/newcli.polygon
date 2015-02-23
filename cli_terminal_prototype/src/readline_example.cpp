#include <algorithm>
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <iostream>
#include <string>
#include <vector>
// C lib
#include <stdio.h>
// select
#include <sys/types.h>
#include <sys/select.h>
// signal
#include <signal.h>
// readline library
#include <readline.h>
#include <history.h>

void init_readline();
int help_key_handler(int count, int ch);
std::string trim_left(std::string const &source);
std::string trim_right(std::string const& source);
std::string trim_full(std::string const& source);

// readline
void clear_input_handler();
void setup_input_handler();
// signals
void setup_signal_handlers();
void signal_handler(int signo);

// completion functions
char** completion_func(const char *text, int start, int end);
char* generator_func(const char *text, int state);

const char *prompt = "readline usage example >>>";
std::vector<std::string> completion_data = {"iddqd666", "idkfa777", "idclip888", "iddqd999"};
bool running = true;

void readline_handler(char *raw_data)
{
    if (raw_data == nullptr)
    {
        std::cout << "exit handler" << std::endl;
        running = false;
        clear_input_handler();
        return;
    }
    std::string raw_str = std::string(raw_data);
    std::string line = trim_full(raw_str);
    if (line.empty())
    {
        free(raw_data);
        return;
    }
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
    free(raw_data);
}

void cleanup()
{
    clear_input_handler();
    rl_deprep_terminal();
}

int main()
{    
    std::cout << "start readline_example" << std::endl;
    init_readline();
    setup_input_handler();
    setup_signal_handlers();
    int instream_no = fileno(rl_instream);
    running = true;
    while(running)
    {
        fd_set fds;
        FD_ZERO(&fds);
        FD_SET(instream_no, &fds);
        int result = select(FD_SETSIZE, &fds, nullptr, nullptr, nullptr);
        if (result == -1)
        {
            if (errno == EINTR)
                continue;
            else
            {
                std::cout << "select error" << std::endl;
                break;
            }
        }
        if (FD_ISSET(instream_no, &fds))
            rl_callback_read_char();
    }
    std::cout << "finish readline_example" << std::endl;
    cleanup();
    return 0;
}

void init_readline()
{
    // singnals
    rl_catch_signals = 0;
    rl_catch_sigwinch = 0;
    // absent in readline 6.2
    // rl_change_environment = 0;
     // history
    using_history();
    // binding keys
    rl_bind_key('?', help_key_handler);
}

int help_key_handler(int count, int ch)
{
    std::string data(rl_line_buffer, rl_end);
    std::cout << "?" << std::endl << "some help for " << data << std::endl;
    rl_delete_text(0, rl_end);
    rl_done = 1;
    return 0;
}

char* duplicate_cstr(std::string const &source)
{
    size_t length = (source.size() + 1) * sizeof(char);
    char *buffer = (char*) malloc(length);
    memcpy(buffer, source.c_str(), length);
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
        return duplicate_cstr(completion_data.at(index++));
    return (char*) NULL;
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

void clear_input_handler()
{
    rl_callback_handler_remove();
    // completion
    rl_attempted_completion_function = nullptr;
}

void setup_input_handler()
{
    rl_attempted_completion_over = 1;
    rl_attempted_completion_function = completion_func;
    rl_sort_completion_matches = 0;
    rl_ignore_completion_duplicates = 0;
    rl_callback_handler_install(prompt, readline_handler);
}

void sigint_handler(int signo)
{
    std::cout << "^C" << std::endl;
    clear_input_handler();
    setup_input_handler();
    setup_signal_handlers();
}

void sigquit_handler(int signo)
{
    std::cout << "^\\" << std::endl;
    running = false;
    clear_input_handler();
    setup_signal_handlers();
}

void sigtstp_handler(int signo)
{
    std::cout << "^Z" << std::endl;
    running = false;
    clear_input_handler();
    setup_signal_handlers();
}

sigset_t create_mask()
{
    sigset_t sigset;
    sigemptyset(&sigset);
    sigaddset(&sigset, SIGINT);
    sigaddset(&sigset, SIGQUIT);
    sigaddset(&sigset, SIGTERM);
    sigaddset(&sigset, SIGHUP);
    sigaddset(&sigset, SIGALRM);
    sigaddset(&sigset, SIGTSTP);
    sigaddset(&sigset, SIGTTIN);
    sigaddset(&sigset, SIGTTOU);
    sigaddset(&sigset, SIGWINCH);
    return sigset;
}

void setup_signal_handler(void (*handler)(int), int signal)
{
    sigset_t signal_mask = create_mask();
    struct sigaction action;
    action.sa_handler = handler;
    action.sa_mask = signal_mask;
    sigaction(signal, &action, nullptr);
}

void setup_signal_handlers()
{
    // sigmask definitions
    sigset_t oldset;
    sigset_t newset = create_mask();
    // signal settings
    sigprocmask(SIG_SETMASK, &newset, &oldset);
    setup_signal_handler(sigint_handler, SIGINT);
    setup_signal_handler(sigquit_handler, SIGQUIT);
    setup_signal_handler(sigtstp_handler, SIGTSTP);
    sigprocmask(SIG_SETMASK, &oldset, nullptr);
}