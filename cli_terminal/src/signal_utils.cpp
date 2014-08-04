#include <unordered_map>
#include "signal.h"
#include "signal_utils.h"

namespace cli_terminal
{

sigset_t create_signal_mask()
{
    sigset_t mask;
    sigemptyset(&mask);
    sigaddset(&mask, SIGINT);
    sigaddset(&mask, SIGQUIT);
    sigaddset(&mask, SIGTERM);
    sigaddset(&mask, SIGHUP);
    sigaddset(&mask, SIGALRM);
    sigaddset(&mask, SIGTSTP);
    sigaddset(&mask, SIGTTIN);
    sigaddset(&mask, SIGTTOU);
    sigaddset(&mask, SIGWINCH);
    return mask;
}

typedef std::unordered_map<int, signal_handler_t>::const_iterator signal_handlers_iterator_t;

void setup_signal_handlers(std::unordered_map<int, signal_handler_t> const &signal_handlers)
{
    sigset_t old_mask;
    sigset_t mask = create_signal_mask();
    pthread_sigmask(SIG_SETMASK, &mask, &old_mask);
    /*int setmask_result = pthread_sigmask(SIG_SETMASK, &mask, &old_mask);
    if (setmask_result != 0)
        throw signal_error();*/
    signal_handlers_iterator_t end = signal_handlers.end();
    for (signal_handlers_iterator_t iterator = signal_handlers.begin(); iterator != end; ++iterator)
    {
        int signal_number = iterator->first;
        signal_handler_t handler = iterator->second;
        struct sigaction signal_action;
        signal_action.sa_handler = handler;
        signal_action.sa_mask = mask;
        sigaction(signal_number, &signal_action, nullptr);
        /*int sigaction_result = sigaction(signal_number, &signal_action, nullptr);
        if (sigaction_result == -1)
            throw signal_error();*/
    }
    pthread_sigmask(SIG_SETMASK, &old_mask, nullptr);
    /*int restoremask_result = pthread_sigmask(SIG_SETMASK, &old_mask, nullptr);
    if (restoremask_result != 0)
        throw signal_error();*/
}

}