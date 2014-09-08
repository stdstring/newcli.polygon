#ifndef H_CLIENT_STATE
#define H_CLIENT_STATE

#include <string>
#include <unordered_map>

namespace cli_terminal
{

enum execution_state {EX_CONTINUE, EX_FINISH};

enum editor_state {ED_INPUT, ED_COMMAND};

struct client_state
{
public:
    client_state() : _socketd(-1), _prompt(""), _ex_state(EX_CONTINUE), _ed_state(ED_INPUT) {}

    // socket descriptor
    int get_socketd() const { return _socketd; }
    void set_socketd(int socketd) { _socketd = socketd; }
    // prompt
    std::string get_prompt() const { return _prompt; }
    void set_prompt(std::string const& prompt) { _prompt = prompt; }
    // execution state
    execution_state get_execution_state() const { return _ex_state; }
    void set_execution_state(execution_state state) { _ex_state = state; }
    // editor state
    editor_state get_editor_state() const { return _ed_state; }
    void set_editor_state(editor_state state) { _ed_state = state; }
    // state parameters
    // ???

private:
    int _socketd;
    std::string _prompt;
    execution_state _ex_state;
    editor_state _ed_state;
    std::unordered_map<std::string, std::string> _state_params;
};

}

#endif