#ifndef H_PROCESS_RESULT
#define H_PROCESS_RESULT

#include "client_state.h"

class ProcessResult
{
public:
    ProcessResult(ExecutionState execution_state_value, EditorState editor_state_value) : execution_state(execution_state_value), editor_state(editor_state_value) {}

    ExecutionState execution_state;
    EditorState editor_state;
};

#endif