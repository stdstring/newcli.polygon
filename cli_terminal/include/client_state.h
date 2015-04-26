#ifndef H_CLIENT_STATE
#define H_CLIENT_STATE

#include <memory>
#include <string>
#include <unordered_map>

#include "execution_state.h"
#include "iterminal_behavior.h"

namespace cli_terminal
{

typedef std::unordered_map<std::string, std::string> state_params_t;

class client_state
{
public:
    client_state() : _socketd(-1), _prompt(""), _ex_state(EX_CONTINUE) {}

    // socket descriptor
    int get_socketd() const { return _socketd; }
    void set_socketd(int socketd) { _socketd = socketd; }
    // prompt
    std::string get_prompt() const { return _prompt; }
    void set_prompt(std::string const& prompt) { _prompt = prompt; }
    // execution state
    execution_state get_execution_state() const { return _ex_state; }
    void set_execution_state(execution_state state) { _ex_state = state; }
    // behavior
    std::shared_ptr<iterminal_behavior> get_behavior() const { return _behavior; }
    void set_behavior(std::shared_ptr<iterminal_behavior> behavior) { _behavior = behavior; }
    // state parameters
    state_params_t& get_params() { return _state_params; }

private:
    int _socketd;
    std::string _prompt;
    execution_state _ex_state;
    std::shared_ptr<iterminal_behavior> _behavior;
    state_params_t _state_params;
};

void set_behavior(client_state &state, std::shared_ptr<iterminal_behavior> behavior);

}

#endif