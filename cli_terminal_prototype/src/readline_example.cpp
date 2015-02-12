#include <algorithm>
#include <cctype>
#include <cstdlib>
#include <cstring>
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
/*char* duplicate_str(std::string const &source);*/
std::string trim_left(std::string const &source);
std::string trim_right(std::string const& source);
std::string trim_full(std::string const& source);

// signals
/*void setup_signal_handlers();
void signal_handler(int signo);*/

// completion functions
char** completion_func(const char *text, int start, int end);
/*char* generator_func(const char *text, int state);*/

const char *prompt = "readline usage example >>>";
std::vector<std::string> completion_data = {"iddqd666", "idkfa777", "idclip888", "iddqd999"};
bool running = true;
//bool skip = false;

void readline_handler(char *raw_data)
{
    if (raw_data == nullptr)
    {
        std::cout << "exit handler" << std::endl;
        running = false;
        rl_callback_handler_remove();
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
    rl_callback_handler_remove();
    rl_deprep_terminal();
}

int main()
{    
    std::cout << "start readline_example" << std::endl;
    init_readline();
    rl_callback_handler_install(prompt, readline_handler);
    /*setup_signal_handlers();*/
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
                rl_callback_handler_remove();
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
    rl_attempted_completion_over = 1;
    rl_attempted_completion_function = completion_func;
    rl_sort_completion_matches = 0;
    rl_ignore_completion_duplicates = 0;
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
    std::cout << "?" << std::endl << "some help for you" << std::endl;
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
    // NULL terminated array
    size_t completion_size = completion_data.size();
    char** completion_array = (char**) malloc((completion_size + 1) * sizeof(char*));
    for(size_t index = 0; index < completion_size; ++index)
        completion_array[index] = duplicate_cstr(completion_data.at(index));
    completion_array[completion_size] = nullptr;
    return completion_array;
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

/*void signal_handler(int signo)
{
    if (signo == SIGINT)
    {
        std::cout << "^C" << std::endl;
        rl_callback_handler_remove();
        rl_callback_handler_install(prompt, readline_handler);
    }
    if (signo == SIGQUIT)
    {
        std::cout << "^\\" << std::endl;
        rl_callback_handler_remove();
        running = false;
    }
    if (signo == SIGWINCH)
    {
        // ??
    }
    if (signo == SIGTSTP)
    {
        std::cout << "^Z" << std::endl;
        rl_callback_handler_remove();
        running = false;
    }
}*/

/*void setup_signal_handlers()
{
    sigset_t oldset, newset;
    sigemptyset(&newset);
    sigaddset(&newset, SIGINT);
    sigaddset(&newset, SIGQUIT);
    sigaddset(&newset, SIGTERM);
    sigaddset(&newset, SIGHUP);
    sigaddset(&newset, SIGALRM);
    sigaddset(&newset, SIGTSTP);
    sigaddset(&newset, SIGTTIN);
    sigaddset(&newset, SIGTTOU);
    sigaddset(&newset, SIGWINCH);
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
}*/