#ifndef H_CLIENT_STATE
#define H_CLIENT_STATE

#include <string>

namespace cli_terminal
{

enum execution_state {EX_CONTINUE, EX_FINISH};

enum editor_state {ED_INPUT, ED_COMMAND};

struct client_state
{
public:
    client_state() : socketd(-1), ex_state(EX_CONTINUE), ed_state(ED_INPUT) {}

    int socketd;
    std::string prompt;
    execution_state ex_state;
    editor_state ed_state;
};

}

#endif