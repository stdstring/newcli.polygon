#ifndef H_MESSAGE_SERIALIZATION
#define H_MESSAGE_SERIALIZATION

#include <string>
#include <vector>

#include "array_ptr.h"
#include "message.h"

namespace cli_terminal
{

typedef array_ptr<unsigned char> byte_array_ptr;

byte_array_ptr serialize(command_request const &request);
byte_array_ptr serialize(interrupt_request const &request);
byte_array_ptr serialize(current_state_request const &request);
byte_array_ptr serialize(extension_request const &request);
byte_array_ptr serialize(exit_request const &request);

template <typename T> T deserialize(byte_array_ptr const &source_data);
template<> message_response deserialize(byte_array_ptr const & source_data);
template<> current_state_response deserialize(byte_array_ptr const & source_data);
template<> extension_response deserialize(byte_array_ptr const & source_data);

}

#endif