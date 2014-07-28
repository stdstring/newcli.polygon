#include <erl_interface.h>
#include <history.h>
#include <readline.h>
#include <unistd.h>
#include "client_state.h"
#include "resource_holder.h"
#include "socket_utils.h"

// TODO (std_string) : think about config port number
#define PORT 22222

// init
void initialize();
// readline
void readline_handler(char *raw_data);
char** completion_func(const char *text, int start, int end);

// global variables
ClientState client_state;

int main()
{
    initialize();
    ResourceHolder<int> socket_holder(create_socket(), [](int socketd){ close(socketd); });
    connect(socket_holder.get(), PORT);
    return 0;
}

void initialize()
{    
    // erl runtime
    erl_init(NULL, 0);
    // readline
    rl_attempted_completion_over = 1;
    // completion
    rl_attempted_completion_function = completion_func;
    rl_sort_completion_matches = 0;
    rl_ignore_completion_duplicates = 0;
    // singnals
    rl_catch_signals = 0;
    rl_catch_sigwinch = 0;
    // absent in readline 6.2
    // rl_change_environment = 0;
    // readline history
    using_history();
}

void readline_handler(char *raw_data)
{}

char** completion_func(const char *text, int start, int end)
{}