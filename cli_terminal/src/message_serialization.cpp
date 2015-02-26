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
    const int tuple_size = 1;
    eterm_ptr type_term(erl_mk_atom(type));
    ETERM* message_data[] = {type_term.get()};
    eterm_ptr message(erl_mk_tuple(message_data, tuple_size));
    return serialize(message);
}

byte_array_ptr serialize(const char *type, const char *body)
{
    const int tuple_size = 2;
    eterm_ptr type_term(erl_mk_atom(type));
    eterm_ptr body_term(erl_mk_string(body));
    ETERM* message_data[] = {type_term.get(), body_term.get()};
    eterm_ptr message(erl_mk_tuple(message_data, tuple_size));
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

byte_array_ptr serialize(help_request const &request)
{
    return serialize(HELP_REQUEST, request.command_line.c_str());
}

byte_array_ptr serialize(suitable_commands_request const &request)
{
    return serialize(SUITABLE_COMMANDS_REQUEST, request.command_line.c_str());
}

byte_array_ptr serialize(mode_exit_request const &request)
{
    return serialize(CURRENT_MODE_EXIT);
}

template<> message_response deserialize(byte_array_ptr const &source_data)
{
    const int type_index = 1;
    const int data_index = 2;
    eterm_ptr eterm(erl_decode(source_data.get()));
    if (!ERL_IS_TUPLE(eterm.get()))
        throw bad_message();
    eterm_ptr type_term(erl_element(type_index, eterm.get()));
    if (!type_term)
        throw bad_message();
    eterm_ptr data_term(erl_element(data_index, eterm.get()));
    if (!data_term)
        throw bad_message();
    std::string type(ERL_ATOM_PTR(type_term.get()));
    cterm_ptr<char> data_ptr(erl_iolist_to_string(data_term.get()));
    std::string data(data_ptr.get());
    return message_response(type, data);
}

template<> current_state_response deserialize(byte_array_ptr const &source_data)
{
    const int type_index = 1;
    const int prompt_index = 2;
    eterm_ptr eterm(erl_decode(source_data.get()));
    if (!ERL_IS_TUPLE(eterm.get()))
        throw bad_message();
    eterm_ptr type_term(erl_element(type_index, eterm.get()));
    if (!type_term)
        throw bad_message();
    std::string type(ERL_ATOM_PTR(type_term.get()));
    if (type.compare(CURRENT_STATE_RESPONSE) != 0)
        throw bad_message();
    eterm_ptr prompt_term(erl_element(prompt_index, eterm.get()));
    if (!prompt_term)
        throw bad_message();
    cterm_ptr<char> prompt_ptr(erl_iolist_to_string(prompt_term.get()));
    std::string prompt(prompt_ptr.get());
    return current_state_response(prompt);
}

template<> extension_response deserialize(byte_array_ptr  const &source_data)
{
    const int type_index = 1;
    const int prefix_index = 2;
    const int extensions_index = 3;
    eterm_ptr eterm(erl_decode(source_data.get()));
    if (!ERL_IS_TUPLE(eterm.get()))
        throw bad_message();
    eterm_ptr type_term(erl_element(type_index, eterm.get()));
    if (!type_term)
        throw bad_message();
    std::string type(ERL_ATOM_PTR(type_term.get()));
    if (type.compare(EXTENSION_RESPONSE) != 0)
        throw bad_message();
    eterm_ptr prefix_term(erl_element(prefix_index, eterm.get()));
    if (!prefix_term)
        throw bad_message();
    cterm_ptr<char> prefix_ptr(erl_iolist_to_string(prefix_term.get()));
    std::string common_prefix(prefix_ptr.get());
    eterm_ptr data_term(erl_element(extensions_index, eterm.get()));
    int list_size = erl_length(data_term.get());
    if (list_size == -1)
        throw bad_message();
    std::vector<std::string> extension_list(list_size);
    ETERM *list = data_term.get();
    while (ERL_IS_CONS(list))
    {
        ETERM *head = ERL_CONS_HEAD(list);
        cterm_ptr<char> extension_data_ptr(erl_iolist_to_string(head));
        extension_list.push_back(std::string(extension_data_ptr.get()));
        list = ERL_CONS_TAIL(list);
    }
    return extension_response(common_prefix, extension_list);
}

template<> help_response deserialize(byte_array_ptr const & source_data)
{
    const int type_index = 1;
    const int help_index = 2;
    eterm_ptr eterm(erl_decode(source_data.get()));
    if (!ERL_IS_TUPLE(eterm.get()))
        throw bad_message();
    eterm_ptr type_term(erl_element(type_index, eterm.get()));
    if (!type_term)
        throw bad_message();
    std::string type(ERL_ATOM_PTR(type_term.get()));
    if (type.compare(HELP_RESPONSE) != 0)
        throw bad_message();
    eterm_ptr data_term(erl_element(help_index, eterm.get()));
    if (!data_term)
        throw bad_message();
    cterm_ptr<char> data_ptr(erl_iolist_to_string(data_term.get()));
    std::string help(data_ptr.get());
    return help_response(help);
}

template<> suitable_commands_response deserialize(byte_array_ptr const & source_data)
{
    const int type_index = 1;
    const int commands_index = 2;
    eterm_ptr eterm(erl_decode(source_data.get()));
    if (!ERL_IS_TUPLE(eterm.get()))
        throw bad_message();
    eterm_ptr type_term(erl_element(type_index, eterm.get()));
    if (!type_term)
        throw bad_message();
    std::string type(ERL_ATOM_PTR(type_term.get()));
    if (type.compare(SUITABLE_COMMANDS_RESPONSE) != 0)
        throw bad_message();
    eterm_ptr data_term(erl_element(commands_index, eterm.get()));
    if (!data_term)
        throw bad_message();
    int list_size = erl_length(data_term.get());
    if (list_size == -1)
        throw bad_message();
    std::vector<std::string> commands_list(list_size);
    ETERM *list = data_term.get();
    while (ERL_IS_CONS(list))
    {
        ETERM *head = ERL_CONS_HEAD(list);
        cterm_ptr<char> command_data_ptr(erl_iolist_to_string(head));
        commands_list.push_back(std::string(command_data_ptr.get()));
        list = ERL_CONS_TAIL(list);
    }
    return suitable_commands_response(commands_list);
}

}