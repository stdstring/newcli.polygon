#include <string>

#include "base64.h"
#include "exception_def.h"

namespace cli_terminal
{

const char footer = '=';

const std::string base64_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

void process_to_base64(std::string& dest, char char1)
{
    // char2 = 0, char3 = 0
    dest.push_back(base64_chars[char1 >> 2]);
    dest.push_back(base64_chars[(char1 & 0x3) << 4]);
    dest.push_back(footer);
    dest.push_back(footer);
}

void process_to_base64(std::string& dest, char char1, char char2)
{
    //char3 = 0
    dest.push_back(base64_chars[char1 >> 2]);
    dest.push_back(base64_chars[((char1 & 0x3) << 4) + (char2 >> 4)]);
    dest.push_back(base64_chars[(char2 & 0xf) << 2]);
    dest.push_back(footer);
}

void process_to_base64(std::string& dest, char char1, char char2, char char3)
{
    dest.push_back(base64_chars[char1 >> 2]);
    dest.push_back(base64_chars[((char1 & 0x3) << 4) + (char2 >> 4)]);
    dest.push_back(base64_chars[((char2 & 0xf) << 2) + (char3 >> 6)]);
    dest.push_back(base64_chars[char3 & 0x3f]);
}

std::string to_base64(std::string const& source)
{
    std::string dest;
    size_t triple_length = 3 * (source.length() / 3);
    for(size_t index = 0; index < triple_length; index += 3)
    {
        char char1 = source.at(index);
        char char2 = source.at(index + 1);
        char char3 = source.at(index + 2);
        process_to_base64(dest, char1, char2, char3);
    }
    switch (source.length() % 3)
    {
        case 1:
            process_to_base64(dest, source.at(source.length() - 1));
            break;
        case 2:
            process_to_base64(dest, source.at(source.length() - 2), source.at(source.length() - 1));
            break;
    }
    return dest;
}

void process_from_base64(std::string& dest, char char1, char char2)
{
    size_t index1 = base64_chars.find(char1);
    if (std::string::npos == index1)
        throw bad_format();
    size_t index2 = base64_chars.find(char2);
    if (std::string::npos == index2)
        throw bad_format();
    dest.push_back((index1 << 2) + (index2 >> 4));
}

void process_from_base64(std::string& dest, char char1, char char2, char char3)
{
    size_t index1 = base64_chars.find(char1);
    if (std::string::npos == index1)
        throw bad_format();
    size_t index2 = base64_chars.find(char2);
    if (std::string::npos == index2)
        throw bad_format();
    size_t index3 = base64_chars.find(char3);
    if (std::string::npos == index3)
        throw bad_format();
    dest.push_back((index1 << 2) + (index2 >> 4));
    dest.push_back(((index2 & 0xf) << 4) + (index3 >> 2));
}

void process_from_base64(std::string& dest, char char1, char char2, char char3, char char4)
{
    size_t index1 = base64_chars.find(char1);
    if (std::string::npos == index1)
        throw bad_format();
    size_t index2 = base64_chars.find(char2);
    if (std::string::npos == index2)
        throw bad_format();
    size_t index3 = base64_chars.find(char3);
    if (std::string::npos == index3)
        throw bad_format();
    size_t index4 = base64_chars.find(char4);
    if (std::string::npos == index4)
        throw bad_format();
    dest.push_back((index1 << 2) + (index2 >> 4));
    dest.push_back(((index2 & 0xf) << 4) + (index3 >> 2));
    dest.push_back(((index3 & 0x3) << 6) + index4);
}

std::string from_base64(std::string const& source)
{
    std::string dest;
    for(size_t index = 0; index < source.length(); index += 4)
    {
        char char1 = source.at(index);
        char char2 = source.at(index + 1);
        char char3 = source.at(index + 2);
        char char4 = source.at(index + 3);
        if ((footer == char3) && (footer == char4))
            process_from_base64(dest, char1, char2);
        else if ((footer != char3) && (footer == char4))
            process_from_base64(dest, char1, char2, char3);
        else
            process_from_base64(dest, char1, char2, char3, char4);
    }
    return dest;
}

}