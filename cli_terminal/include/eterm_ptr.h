#ifndef H_ETERM_PTR
#define H_ETERM_PTR

#include <memory>
#include <erl_interface.h>

namespace cli_terminal
{

typedef std::unique_ptr<ETERM, std::function<void (ETERM*)>> eterm_unique_ptr;

class eterm_ptr
{
public:
    eterm_ptr() = delete;
    eterm_ptr(const eterm_ptr&) = delete;
    eterm_ptr(eterm_ptr&&) = delete;
    eterm_ptr& operator=(const eterm_ptr&) = delete;
    eterm_ptr& operator=(eterm_ptr&&) = delete;
    explicit eterm_ptr(ETERM *eterm) noexcept : _eterm(eterm_unique_ptr(eterm, [](ETERM* term){ erl_free_term(term); })) {}
    ~eterm_ptr() {}

    ETERM* get() const noexcept { return _eterm.get(); }
    explicit operator bool() const noexcept { return get() != nullptr; }
private:
    eterm_unique_ptr _eterm;
};

}

#endif