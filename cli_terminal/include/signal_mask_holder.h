#ifndef H_SIGNAL_MASK_HOLDER
#define H_SIGNAL_MASK_HOLDER

#include <signal.h>

class SignalMaskHolder
{
public:
    SignalMaskHolder() = delete;
    SignalMaskHolder(const SignalMaskHolder&) = delete;
    SignalMaskHolder(SignalMaskHolder&&) = delete;
    SignalMaskHolder& operator=(const SignalMaskHolder&) = delete;
    SignalMaskHolder& operator=(SignalMaskHolder&&) = delete;

    SignalMaskHolder(sigset_t new_mask)
    {
        sigprocmask(SIG_SETMASK, &new_mask, &_old_mask);
    }

    ~SignalMaskHolder()
    {
        sigprocmask(SIG_SETMASK, &_old_mask, nullptr);
    }
private:
    sigset_t _old_mask;
};

#endif