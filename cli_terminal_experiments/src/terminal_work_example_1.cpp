#include <iostream>
#include <vector>
#include <map>
#include <string>

#include <unistd.h>
#include <termios.h>

void check_terminal();
termios get_term_attrs();
void set_term_attrs(termios const &attrs);
termios construct_noncanon_mode(termios const &source_attrs);
void check_noncanon_mode();
void main_loop();
void show_keycodes(char buf[], int length);
void show_keychars(char buf[], int length);

typedef std::vector<char> cseq;

std::map<cseq, std::string> known_cseqs =
    {
        // right arrow
        {{27, 91, 67}, "right arrow"},
        // crtl + F
        {{6}, "ctrl + F"},
    };

int main()
{
    std::cout << "cli_terminal_example start" << std::endl;
    check_terminal();
    termios old_attrs = get_term_attrs();
    termios new_attrs = construct_noncanon_mode(old_attrs);
    set_term_attrs(new_attrs);
    check_noncanon_mode();
    main_loop();
    set_term_attrs(old_attrs);
    std::cout << "cli_terminal_example finish" << std::endl;
    return 0;
}

void check_terminal()
{
    if (!isatty(STDIN_FILENO))
        exit(-1);
    std::cout << "0 is terminal" << std::endl;
}

termios get_term_attrs()
{
    termios buf;
    if (tcgetattr(STDIN_FILENO, &buf) < 0)
        exit(-1);
    return buf;
}

void set_term_attrs(termios const &attrs)
{
    if (tcsetattr(STDIN_FILENO, TCSANOW, &attrs) < 0)
        exit(-1);
}

termios construct_noncanon_mode(termios const &source_attrs)
{
    termios dest_attrs(source_attrs);
    dest_attrs.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    dest_attrs.c_iflag &= ~(BRKINT | ICRNL | ISTRIP | IXON);
    dest_attrs.c_cc[VMIN] = 1;
    dest_attrs.c_cc[VTIME] = 0;
    return dest_attrs;
}

void check_noncanon_mode()
{
    termios attrs = get_term_attrs();
    if ((attrs.c_lflag & (ECHO | ICANON | IEXTEN | ISIG)) ||
        (attrs.c_iflag & (BRKINT | ICRNL | ISTRIP | IXON)) || 
        attrs.c_cc[VMIN] != 1 ||
        attrs.c_cc[VTIME] != 0)
        exit(-1);
}

void main_loop()
{
    char buf[100];
    int length;
    std::map<cseq, std::string>::const_iterator known_cseqs_end = known_cseqs.end();
    while((length = read(STDIN_FILENO, &buf, 100)) > 0)
    {
        std::cout << "Pressed: ";
        cseq seq(&buf[0], &buf[length]);
        if (known_cseqs_end != known_cseqs.find(seq))
            std::cout << known_cseqs[seq] << std::endl;
        else
        {
            show_keycodes(buf, length);
            std::cout << "Char: ";
            show_keychars(buf, length);
        }
        if (length == 1 && (buf[0] &= 255) == 0177)
            break;
    }
}

void show_keycodes(char buf[], int length)
{
    for (int i = 0; i < length; ++i)
        std::cout << (unsigned int)buf[i] << " ";
    std::cout << std::endl;
}

void show_keychars(char buf[], int length)
{
    for (int i = 0; i < length; ++i)
        std::cout << (char)buf[i] << " ";
    std::cout << std::endl;
}