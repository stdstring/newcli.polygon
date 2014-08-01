#ifndef H_CLIENT_STATE
#define H_CLIENT_STATE

#include <string>

enum ExecutionState {EX_CONTINUE, EX_FINISH};

enum EditorState {ED_INPUT, ED_COMMAND};

struct client_state
{
public:
    client_state() : socketd(-1), execution_state(EX_CONTINUE), editor_state(ED_INPUT) {}

    int socketd;
    std::string prompt;
    ExecutionState execution_state;
    EditorState editor_state;
};

#endif