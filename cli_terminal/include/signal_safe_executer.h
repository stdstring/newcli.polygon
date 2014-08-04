#ifndef H_SIGNAL_SAFE_EXECUTER
#define H_SIGNAL_SAFE_EXECUTER

#include <functional>
#include <signal.h>
#include "signal_mask_holder.h"

namespace cli_terminal
{

class signal_safe_executer
{
public:
    signal_safe_executer() = delete;
    signal_safe_executer(const signal_safe_executer&) = delete;
    signal_safe_executer(signal_safe_executer&&) = delete;
    signal_safe_executer& operator=(const signal_safe_executer&) = delete;
    signal_safe_executer& operator=(signal_safe_executer&&) = delete;

    signal_safe_executer(sigset_t mask) : _mask(mask) {}
    ~signal_safe_executer() {}

    /*template<class Ret, class... Args> Ret execute(std::function<Ret(Args...)> func, Args... args)
    {
        signal_mask_holder signal_holder(_mask);
        return func(&args...);
    }

    template<class... Args> void execute(std::function<void(Args...)> func, Args... args)
    {
        signal_mask_holder signal_holder(_mask);
        func(&args...);
    }*/

    template<class Ret> Ret execute(std::function<Ret()> func)
    {
        signal_mask_holder signal_holder(_mask);
        return func();
    }

    void execute(std::function<void()> func)
    {
        signal_mask_holder signal_holder(_mask);
        func();
    }
private:
    sigset_t _mask;
};

}

#endif