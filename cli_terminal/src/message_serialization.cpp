#include <cstring>
#include <memory>
#include <erl_interface.h>
#include "cterm_ptr.h"
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

byte_array_ptr serialize(ExtensionRequest const &request)
{
    return serialize("extension", request.command_line.c_str());
}

template<> MessageResponse deserialize(byte_array_ptr source_data)
{
    eterm_ptr eterm(erl_decode(source_data.get()));
    /*if (!ERL_IS_TUPLE(eterm.get()))
        throw bad_message();*/
    eterm_ptr type_term(erl_element(1, eterm.get()));
    /*if (!type)
        throw bad_message();*/
    eterm_ptr data_term(erl_element(2, eterm.get()));
    /*if (!data)
        throw bad_message();*/
    std::string type(ERL_ATOM_PTR(type_term.get()));
    cterm_ptr<char> data_ptr(erl_iolist_to_string(data_term.get()));
    std::string data(data_ptr.get());
    return MessageResponse(type, data);
};

template<> ExtensionResponse deserialize(byte_array_ptr source_data)
{
    eterm_ptr eterm(erl_decode(source_data.get()));
    /*if (!ERL_IS_TUPLE(eterm.get()))
        throw bad_message();*/
    /*eterm_ptr type_term(erl_element(1, eterm.get()));
    if (!type)
        throw bad_message();
    std::string type(ERL_ATOM_PTR(type_term.get()));
    if (type.compare("expansion") != 0)
        throw bad_message();*/
    eterm_ptr data_term(erl_element(2, eterm.get()));
    /*if (!data)
        throw bad_message();*/
    int expansion_list_size = erl_length(data_term.get());
    /*if (expansion_list_size == -1)
        throw bad_message();*/
    std::vector<std::string> extension_list(expansion_list_size);
    ETERM *list = data_term.get();
    while (list != nullptr)
    {
        ETERM *head = ERL_CONS_HEAD(list);
        cterm_ptr<char> extension_data_ptr(erl_iolist_to_string(head));
        extension_list.push_back(std::string(extension_data_ptr.get()));
        list = ERL_CONS_TAIL(list);
    }
    return ExtensionResponse(extension_list);
};