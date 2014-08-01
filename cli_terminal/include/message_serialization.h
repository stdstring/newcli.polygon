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

byte_array_ptr serialize(command_request const &request);
byte_array_ptr serialize(interrupt_request const &request);
byte_array_ptr serialize(current_state_request const &request);
byte_array_ptr serialize(extension_request const &request);

template <typename T> T deserialize(byte_array_ptr const &source_data);
template<> message_response deserialize(byte_array_ptr const & source_data);
template<> extension_response deserialize(byte_array_ptr const & source_data);

#endif