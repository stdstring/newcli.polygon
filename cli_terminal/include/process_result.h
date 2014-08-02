#ifndef H_PROCESS_RESULT
#define H_PROCESS_RESULT

#include "client_state.h"

class process_result
{
public:
    process_result(execution_state ex_state_value, editor_state ed_state_value) : ex_state(ex_state_value), ed_state(ed_state_value) {}

    execution_state ex_state;
    editor_state ed_state;
};

#endif