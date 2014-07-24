#ifndef H_MESSAGE_SERIALIZATION
#define H_MESSAGE_SERIALIZATION

//#include <memory>
#include <string>
#include <vector>
#include "message.h"

//typedef std::unique_ptr<unsigned char[]> byte_array_ptr;
//typedef std::vector<unsigned char> byte_array;

byte_array_ptr serialize(CommandRequest const &request);
byte_array_ptr serialize(InterruptRequest const &request);
byte_array_ptr serialize(CurrentStateRequest const &request);
byte_array_ptr serialize(ExtensionRequest const &request);

template <typename T> T deserialize(byte_array_ptr source_data);
template<> MessageResponse deserialize(byte_array_ptr source_data);
template<> ExtensionResponse deserialize(byte_array_ptr source_data);

#endif