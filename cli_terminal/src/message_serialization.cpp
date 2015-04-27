//#include <cstring>
#include <functional>
#include <memory>
#include <erl_interface.h>

#include "cterm_ptr.h"
#include "eterm_ptr.h"
#include "exception_def.h"
#include "message.h"
#include "message_serialization.h"

#include <iostream>

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

byte_array_ptr serialize(std::string const &type)
{
    const int tuple_size = 1;
    eterm_ptr type_term(erl_mk_atom(type.c_str()));
    ETERM* message_data[] = {type_term.get()};
    eterm_ptr message(erl_mk_tuple(message_data, tuple_size));
    return serialize(message);
}

byte_array_ptr serialize(std::string const &type, std::string const &body)
{
    const int tuple_size = 2;
    eterm_ptr type_term(erl_mk_atom(type.c_str()));
    eterm_ptr body_term(erl_mk_string(body.c_str()));
    ETERM* message_data[] = {type_term.get(), body_term.get()};
    eterm_ptr message(erl_mk_tuple(message_data, tuple_size));
    return serialize(message);
}

byte_array_ptr serialize(command_request const &request)
{
    return serialize(command_start_tag, request.command_line);
}

byte_array_ptr serialize(interrupt_request const &request)
{
    return serialize(command_stop_tag);
}

byte_array_ptr serialize(current_state_request const &request)
{
    return serialize(current_state_request_tag);
}

byte_array_ptr serialize(extension_request const &request)
{
    return serialize(extension_request_tag, request.command_line);
}

byte_array_ptr serialize(exit_request const &request)
{
    return serialize(exit_tag);
}

byte_array_ptr serialize(help_request const &request)
{
    return serialize(help_request_tag, request.command_line);
}

byte_array_ptr serialize(suitable_commands_request const &request)
{
    return serialize(suitable_commands_request_tag, request.command_line);
}

byte_array_ptr serialize(mode_exit_request const &request)
{
    return serialize(current_mode_exit_tag);
}

byte_array_ptr serialize(login_request const &request)
{
    const int tuple_size = 3;
    eterm_ptr tag_term(erl_mk_atom(login_request_tag.c_str()));
    eterm_ptr username_term(erl_mk_string(request.username.c_str()));
    eterm_ptr password_term(erl_mk_string(request.password.c_str()));
    ETERM* message_data[] = {tag_term.get(), username_term.get(), password_term.get()};
    eterm_ptr message(erl_mk_tuple(message_data, tuple_size));
    return serialize(message);
}

std::string extract_atom(eterm_ptr &term)
{
    if (!term)
        throw bad_message();
    return std::string(ERL_ATOM_PTR(term.get()));
}

std::string extract_string(eterm_ptr &term)
{
    if (!term)
        throw bad_message();
    cterm_ptr<char> data_ptr(erl_iolist_to_string(term.get()));
    return std::string(data_ptr.get());
}

std::string extract_string(ETERM *term)
{
    eterm_ptr eterm(term);
    return extract_string(eterm);
}

template <typename T> std::vector<T> extract_list(eterm_ptr &term, std::function<T(ETERM*)> factory)
{
    if (!term)
        throw bad_message();
    int list_size = erl_length(term.get());
    if (list_size == -1)
        throw bad_message();
    std::vector<T> dest;
    ETERM *list = term.get();
    while (ERL_IS_CONS(list))
    {
        ETERM *head = ERL_CONS_HEAD(list);
        dest.push_back(factory(head));
        list = ERL_CONS_TAIL(list);
    }
    return dest;
}

void check_type(eterm_ptr &eterm, std::string const &expexted_type)
{
    std::string type = extract_atom(eterm);
    if (type.compare(expexted_type) != 0)
        throw bad_message();
}

template<> message_response deserialize(byte_array_ptr const &source_data)
{
    const int type_index = 1;
    const int data_index = 2;
    eterm_ptr eterm(erl_decode(source_data.get()));
    if (!ERL_IS_TUPLE(eterm.get()))
        throw bad_message();
    eterm_ptr type_term(erl_element(type_index, eterm.get()));
    std::string type = extract_atom(type_term);
    eterm_ptr data_term(erl_element(data_index, eterm.get()));
    std::string data = extract_string(data_term);
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
    check_type(type_term, current_state_response_tag);
    eterm_ptr prompt_term(erl_element(prompt_index, eterm.get()));
    std::string prompt = extract_string(prompt_term);
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
    check_type(type_term, extension_response_tag);
    eterm_ptr prefix_term(erl_element(prefix_index, eterm.get()));
    std::string common_prefix = extract_string(prefix_term);
    eterm_ptr data_term(erl_element(extensions_index, eterm.get()));
    std::vector<std::string> extension_list = extract_list<std::string>(data_term, [](ETERM *term){ return extract_string(term); });
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
    check_type(type_term, help_response_tag);
    eterm_ptr data_term(erl_element(help_index, eterm.get()));
    std::string help = extract_string(data_term);
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
    check_type(type_term, suitable_commands_response_tag);
    eterm_ptr data_term(erl_element(commands_index, eterm.get()));
    std::vector<std::string> commands_list = extract_list<std::string>(data_term, [](ETERM *term){ return extract_string(term); });
    return suitable_commands_response(commands_list);
}

template<> login_response deserialize(byte_array_ptr const & source_data)
{
    const int type_index = 1;
    const int data_index = 2;
    eterm_ptr eterm(erl_decode(source_data.get()));
    if (!ERL_IS_TUPLE(eterm.get()))
        throw bad_message();
    eterm_ptr type_term(erl_element(type_index, eterm.get()));
    std::string type = extract_string(type_term);
    eterm_ptr data_term(erl_element(data_index, eterm.get()));
    std::string data = extract_string(data_term);
    return login_response(type, data);
}

}