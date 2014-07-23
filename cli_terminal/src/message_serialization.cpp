#include <cstring>
#include <memory>
#include <erl_interface.h>
#include "eterm_ptr.h"
#include "message.h"
#include "message_serialization.h"

/*#define LENGTH_SIZE 4*/

/*byte_array_ptr serialize(eterm_ptr const &eterm)
{
    int length = erl_term_len(eterm.get());
    // buffer size = 4-byte length + term length
    int total_length = length + LENGTH_SIZE;
    byte_array_ptr buffer(new unsigned char[total_length]);
    if (erl_encode(eterm.get(), (buffer.get() + LENGTH_SIZE)) == 0)
        return byte_array_ptr();
    int length_binary = htonl(length);
    memcpy(buffer.get(), &length_binary, 4);
    return buffer;
}*/

byte_array_ptr serialize(eterm_ptr const &eterm)
{
    int length = erl_term_len(eterm.get());
    byte_array_ptr buffer(new unsigned char[length]);
    if (erl_encode(eterm.get(), (buffer.get())) == 0)
        return byte_array_ptr();
    return buffer;
}

byte_array_ptr serialize(const char *type)
{
    eterm_ptr type_term(erl_mk_atom(type));
    ETERM* message_data[] = {type_term.get()};
    eterm_ptr message(erl_mk_tuple(message_data, 1));
    return serialize(message);
}

byte_array_ptr serialize(const char *type, const char *body)
{
    eterm_ptr type_term(erl_mk_atom(type));
    eterm_ptr body_term(erl_mk_string(body));
    ETERM* message_data[] = {type_term.get(), body_term.get()};
    eterm_ptr message(erl_mk_tuple(message_data, 2));
    return serialize(message);
}

byte_array_ptr serialize(CommandRequest const &request)
{
    return serialize("command", request.command_line.c_str());
}

byte_array_ptr serialize(InterruptRequest const &request)
{
    return serialize("interrupt");
}

byte_array_ptr serialize(CurrentStateRequest const &request)
{
    return serialize("current_state");
}

byte_array_ptr serialize(ExpansionRequest const &request)
{
    return serialize("expansion", request.command_line.c_str());
}

template<> MessageResponse deserialize(byte_array_ptr source_data)
{
    eterm_ptr eterm(erl_decode(source_data.get()));
    return MessageResponse("", "");
};

template<> ExpansionResponse deserialize(byte_array_ptr source_data)
{
    return ExpansionResponse(std::vector<std::string>());
};