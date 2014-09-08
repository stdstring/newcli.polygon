#include <cstring>
#include <memory>
#include <erl_interface.h>

#include "cterm_ptr.h"
#include "eterm_ptr.h"
#include "exception_def.h"
#include "message.h"
#include "message_serialization.h"

namespace cli_terminal
{

byte_array_ptr serialize(eterm_ptr const &eterm)
{
    int length = erl_term_len(eterm.get());
    byte_array_ptr buffer(new unsigned char[length], length);
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

byte_array_ptr serialize(command_request const &request)
{
    return serialize(COMMAND_START, request.command_line.c_str());
}

byte_array_ptr serialize(interrupt_request const &request)
{
    return serialize(COMMAND_STOP);
}

byte_array_ptr serialize(current_state_request const &request)
{
    return serialize(CURRENT_STATE_REQUEST);
}

byte_array_ptr serialize(extension_request const &request)
{
    return serialize(EXTENSION_REQUEST, request.command_line.c_str());
}

byte_array_ptr serialize(exit_request const &request)
{
    return serialize(EXIT);
}

template<> message_response deserialize(byte_array_ptr const &source_data)
{
    eterm_ptr eterm(erl_decode(source_data.get()));
    if (!ERL_IS_TUPLE(eterm.get()))
        throw bad_message();
    eterm_ptr type_term(erl_element(1, eterm.get()));
    if (!type_term)
        throw bad_message();
    eterm_ptr data_term(erl_element(2, eterm.get()));
    if (!data_term)
        throw bad_message();
    std::string type(ERL_ATOM_PTR(type_term.get()));
    cterm_ptr<char> data_ptr(erl_iolist_to_string(data_term.get()));
    std::string data(data_ptr.get());
    return message_response(type, data);
};

template<> current_state_response deserialize(byte_array_ptr const &source_data)
{
    eterm_ptr eterm(erl_decode(source_data.get()));
    if (!ERL_IS_TUPLE(eterm.get()))
        throw bad_message();
    if (!ERL_IS_TUPLE(eterm.get()))
        throw bad_message();
    eterm_ptr type_term(erl_element(1, eterm.get()));
    if (!type_term)
        throw bad_message();
    std::string type(ERL_ATOM_PTR(type_term.get()));
    if (type.compare(CURRENT_STATE_RESPONSE) != 0)
        throw bad_message();
    eterm_ptr prompt_term(erl_element(2, eterm.get()));
    if (!prompt_term)
        throw bad_message();
    cterm_ptr<char> prompt_ptr(erl_iolist_to_string(prompt_term.get()));
    std::string prompt(prompt_ptr.get());
    return current_state_response(prompt);
};

template<> extension_response deserialize(byte_array_ptr  const &source_data)
{
    eterm_ptr eterm(erl_decode(source_data.get()));
    if (!ERL_IS_TUPLE(eterm.get()))
        throw bad_message();
    eterm_ptr type_term(erl_element(1, eterm.get()));
    if (!type_term)
        throw bad_message();
    std::string type(ERL_ATOM_PTR(type_term.get()));
    if (type.compare(EXTENSION_RESPONSE) != 0)
        throw bad_message();
    eterm_ptr data_term(erl_element(2, eterm.get()));
    if (!data_term)
        throw bad_message();
    int expansion_list_size = erl_length(data_term.get());
    if (expansion_list_size == -1)
        throw bad_message();
    std::vector<std::string> extension_list(expansion_list_size);
    ETERM *list = data_term.get();
    while (list != nullptr)
    {
        ETERM *head = ERL_CONS_HEAD(list);
        cterm_ptr<char> extension_data_ptr(erl_iolist_to_string(head));
        extension_list.push_back(std::string(extension_data_ptr.get()));
        list = ERL_CONS_TAIL(list);
    }
    return extension_response(extension_list);
};

}