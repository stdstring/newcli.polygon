#ifndef H_MESSAGE_SERIALIZATION
#define H_MESSAGE_SERIALIZATION

#include <memory>
#include "cterm_ptr.h"
#include "message.h"

typedef std::unique_ptr<unsigned char[]> byte_array_ptr;

 byte_array_ptr serialize(CommandRequest const &request);

#endif