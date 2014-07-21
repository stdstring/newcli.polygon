#ifndef H_SIGNAL_SAFE_EXECUTER
#define H_SIGNAL_SAFE_EXECUTER

#include <functional>
#include <signal.h>
#include "signal_mask_holder.h"

class SignalSafeExecuter
{
public:
    SignalSafeExecuter() = delete;
    SignalSafeExecuter(const SignalSafeExecuter&) = delete;
    SignalSafeExecuter(SignalSafeExecuter&&) = delete;
    SignalSafeExecuter& operator=(const SignalSafeExecuter&) = delete;
    SignalSafeExecuter& operator=(SignalSafeExecuter&&) = delete;

    SignalSafeExecuter(sigset_t mask) : _mask(mask) {}
    ~SignalSafeExecuter() {}

    /*template<class Ret, class... Args> Ret execute(std::function<Ret(Args...)> func, Args... args)
    {
        SignalMaskHolder signalHolder(_mask);
        return func(&args...);
    }

    template<class... Args> void execute(std::function<void(Args...)> func, Args... args)
    {
        SignalMaskHolder signalHolder(_mask);
        func(&args...);
    }*/

    template<class Ret> Ret execute(std::function<Ret()> func)
    {
        SignalMaskHolder signalHolder(_mask);
        return func();
    }

    void execute(std::function<void()> func)
    {
        SignalMaskHolder signalHolder(_mask);
        func();
    }
private:
    sigset_t _mask;
};

#endif