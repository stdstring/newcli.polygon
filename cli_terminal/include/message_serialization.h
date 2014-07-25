#ifndef H_MESSAGE_SERIALIZATION
#define H_MESSAGE_SERIALIZATION

#include <cstddef>
#include <memory>
#include <string>
#include <vector>
#include "message.h"

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

typedef array_ptr<unsigned char> byte_array_ptr;

byte_array_ptr serialize(CommandRequest const &request);
byte_array_ptr serialize(InterruptRequest const &request);
byte_array_ptr serialize(CurrentStateRequest const &request);
byte_array_ptr serialize(ExtensionRequest const &request);

template <typename T> T deserialize(byte_array_ptr source_data);
template<> MessageResponse deserialize(byte_array_ptr source_data);
template<> ExtensionResponse deserialize(byte_array_ptr source_data);

#endif