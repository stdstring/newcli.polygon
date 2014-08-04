#ifndef H_SIGNAL_MASK_HOLDER
#define H_SIGNAL_MASK_HOLDER

#include <signal.h>

namespace cli_terminal
{

class signal_mask_holder
{
public:
    signal_mask_holder() = delete;
    signal_mask_holder(const signal_mask_holder&) = delete;
    signal_mask_holder(signal_mask_holder&&) = delete;
    signal_mask_holder& operator=(const signal_mask_holder&) = delete;
    signal_mask_holder& operator=(signal_mask_holder&&) = delete;

    signal_mask_holder(sigset_t new_mask)
    {
        sigprocmask(SIG_SETMASK, &new_mask, &_old_mask);
    }

    ~signal_mask_holder()
    {
        sigprocmask(SIG_SETMASK, &_old_mask, nullptr);
    }
private:
    sigset_t _old_mask;
};

}

#endif