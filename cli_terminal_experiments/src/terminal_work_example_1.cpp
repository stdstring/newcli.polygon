#include <iostream>
#include <unistd.h>
#include <termios.h>

#define FD_INPUT 0

void check_terminal();
termios get_term_attrs();
void set_term_attrs(termios const &attrs);
termios construct_noncanon_mode(termios const &source_attrs);
void check_noncanon_mode();

int main()
{
    std::cout << "cli_terminal_example" << std::endl;
    check_terminal();
    termios old_attrs = get_term_attrs();
    termios new_attrs = construct_noncanon_mode(old_attrs);
    set_term_attrs(new_attrs);
    check_noncanon_mode();
    set_term_attrs(old_attrs);
    return 0;
}

void check_terminal()
{
    if (!isatty(FD_INPUT))
        exit(-1);
    std::cout << "0 is terminal" << std::endl;
}

termios get_term_attrs()
{
    termios buf;
    if (tcgetattr(FD_INPUT, &buf) < 0)
        exit(-1);
    return buf;
}

void set_term_attrs(termios const &attrs)
{
    if (tcsetattr(FD_INPUT, TCSANOW, &attrs) < 0)
        exit(-1);
}

termios construct_noncanon_mode(termios const &source_attrs)
{
    termios dest_attrs(source_attrs);
    dest_attrs.c_lflag &= ~(ECHO | ICANON);
    dest_attrs.c_cc[VMIN] = 1;
    dest_attrs.c_cc[VTIME] = 0;
    return dest_attrs;
}

void check_noncanon_mode()
{
    termios attrs = get_term_attrs();
    if ((attrs.c_lflag & (ECHO | ICANON)) || attrs.c_cc[VMIN] != 1 || attrs.c_cc[VTIME] != 0)
        exit(-1);
}