#ifndef H_CLIENT_STATE
#define H_CLIENT_STATE

enum ExecutionState {EX_CONTINUE, EX_FINISH};

enum EditorState {ED_INPUT, ED_COMMAND};

struct ClientState
{
public:
    ClientState() : socketd(-1), execution_state(EX_CONTINUE), editor_state(ED_INPUT) {}

    int socketd;
    ExecutionState execution_state;
    EditorState editor_state;
};

#endif