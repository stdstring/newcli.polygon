#ifndef H_CTERM_PTR
#define H_CTERM_PTR

#include <memory>

template <typename T> class cterm_ptr
{
public:
    cterm_ptr() = delete;
    cterm_ptr(const cterm_ptr&) = delete;
    cterm_ptr(cterm_ptr&&) = delete;
    cterm_ptr& operator=(const cterm_ptr&) = delete;
    cterm_ptr& operator=(cterm_ptr&&) = delete;
    explicit cterm_ptr(T *term) noexcept : _term(std::unique_ptr<T, std::function<void (T*)>>(term, [](T* t){ free(t); })) {}
    ~cterm_ptr() {}

    T* get() const noexcept { return _term.get(); }
    explicit operator bool() const noexcept { return get() != nullptr; }

private:
    std::unique_ptr<T, std::function<void (T*)>> _term;
};

#endif