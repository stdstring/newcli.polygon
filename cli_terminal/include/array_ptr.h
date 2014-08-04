#ifndef H_ARRAY_PTR
#define H_ARRAY_PTR

#include <memory>
#include <cstddef>

namespace cli_terminal
{

template <typename T> class array_ptr
{
public:
    array_ptr(const array_ptr&) = delete;
    array_ptr& operator=(const array_ptr&) = delete;

    constexpr array_ptr() noexcept = default;
    constexpr array_ptr (nullptr_t) noexcept : array_ptr() {}
    explicit array_ptr(T array[], size_t array_size) noexcept : _array(std::unique_ptr<T[]>(array)), _array_size(array_size) {}
    array_ptr(array_ptr<T>&& other) noexcept = default;
    array_ptr& operator=(array_ptr&&) = default;
    ~array_ptr() {}

    T* get() const noexcept { return _array.get(); }
    explicit operator bool() const noexcept { return get() != nullptr; }
    size_t size() const { return _array_size; }
private:
    std::unique_ptr<T[]> _array;
    size_t _array_size;
};

}

#endif