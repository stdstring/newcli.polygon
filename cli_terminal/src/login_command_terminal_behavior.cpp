#include <functional>
#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>

#include <readline.h>
#include "signal.h"

#include "cli_io_helper.h"
#include "client_state.h"
#include "cterm_ptr.h"
#include "empty_terminal_behavior.h"
#include "execution_state.h"
#include "iterminal_behavior.h"
#include "login_command_terminal_behavior.h"
#include "message.h"
#include "server_interaction_helper.h"
#include "signal_safe_executer.h"
#include "signal_utils.h"
#include "string_utils.h"

namespace cli_terminal
{

extern client_state cstate;

const std::string login_key = "login";
const std::string login_prompt = "login:";
const std::string password_prompt = "password:";

namespace login_command_terminal_behavior_impl
{

void install_signal_handlers();
void clear_input_handler();
void install_input_handler();
void install_login_input_handler();
void install_password_input_handler();


void sigint_handler(int signo)
{
    // TODO (std_string) : may be use rl_echo_signal_char(int)
    std::cout << "^C" << std::endl;
    clear_input_handler();
    // remove login parameter
    state_params_t &state_params = cstate.get_params();
    state_params.erase(login_key);
    // login terminal behavior
    set_behavior(cstate, std::shared_ptr<iterminal_behavior>(new login_command_terminal_behavior()));
}

void sigquit_handler(int signo)
{
    // TODO (std_string) : may be use rl_echo_signal_char(int)
    std::cout << "^\\" << std::endl;
    cstate.set_execution_state(EX_FINISH);
    clear_input_handler();
    install_signal_handlers();
}

void sigwinch_handler(int signo)
{
    // TODO (std_string) : may be some reaction
    clear_input_handler();
    install_input_handler();
    install_signal_handlers();
}

void sigtstp_handler(int signo)
{
    // TODO (std_string) : may be use rl_echo_signal_char(int)
    std::cout << "^Z" << std::endl;
    cstate.set_execution_state(EX_FINISH);
    clear_input_handler();
    install_signal_handlers();
}

void install_signal_handlers()
{
    std::unordered_map<int, signal_handler_t> handlers =
        {{SIGINT, sigint_handler}, {SIGQUIT, sigquit_handler}, {SIGWINCH, sigwinch_handler}, {SIGTSTP, sigtstp_handler}};
    setup_signal_handlers(handlers);
}

void password_redisplay(void)
{
    // do nothing now
}

typedef std::function<void(login_response const&, client_state&)> login_response_handler_t;
typedef std::unordered_map<std::string, login_response_handler_t> login_response_handlers_def_t;

void process_login(std::string const &username, std::string const &password)
{
    login_response_handler_t login_success_handler = [](login_response const &response, client_state &state){
        std::cout << response.data;
        set_behavior(state, std::shared_ptr<iterminal_behavior>(new empty_terminal_behavior()));
        state.set_execution_state(EX_CONTINUE);
    };
    login_response_handler_t login_fail_handler = [](login_response const &response, client_state &state){
        std::cout << response.data;
        set_behavior(state, std::shared_ptr<iterminal_behavior>(new login_command_terminal_behavior()));
        state.set_execution_state(EX_CONTINUE);
    };
    login_response_handler_t login_error_handler = [](login_response const &response, client_state &state){
        std::cerr << response.data;
        set_behavior(state, std::shared_ptr<iterminal_behavior>(new empty_terminal_behavior()));
        state.set_execution_state(EX_FINISH);
    };
    login_response_handlers_def_t handlers = {
        {login_success_response_tag, login_success_handler},
        {login_fail_response_tag, login_fail_handler},
        {login_error_response_tag, login_error_handler}
    };
    login_response response = login(cstate.get_socketd(), username, password);
    login_response_handler_t handler = handlers.at(response.type);
    handler(response, cstate);
}

void login_input_handler_impl(char *raw_data)
{
    if (nullptr == raw_data)
    {
        cstate.set_execution_state(EX_FINISH);
        clear_input_handler();
        return;
    }
    cterm_ptr<char> raw_data_ptr(raw_data);
    std::string login = trim_full(raw_data_ptr.get());
    // add login into client_state parameters
    state_params_t &state_params = cstate.get_params();
    state_params.emplace(login_key, login);
    // install password_input_handler
    clear_input_handler();
    install_password_input_handler();
}

void password_input_handler_impl(char *raw_data)
{
    if (nullptr == raw_data)
    {
        cstate.set_execution_state(EX_FINISH);
        clear_input_handler();
        return;
    }
    cterm_ptr<char> raw_data_ptr(raw_data);
    std::string password(raw_data_ptr.get());
    // extract and remove login from client_state parameters
    state_params_t &state_params = cstate.get_params();
    std::string username = state_params.at(login_key);
    state_params.erase(login_key);
    // process login
    process_login(username, password);
}

void login_input_handler(char *raw_data)
{
    signal_safe_executer executer(create_signal_mask());
    std::function<void()> func = [raw_data](){ login_input_handler_impl(raw_data); };
    executer.execute(func);
}

void password_input_handler(char *raw_data)
{
    signal_safe_executer executer(create_signal_mask());
    std::function<void()> func = [raw_data](){ password_input_handler_impl(raw_data); };
    executer.execute(func);
}

void clear_input_handler()
{
    rl_callback_handler_remove();
    // set default redisplay function
    rl_redisplay_function = rl_redisplay;
}

void install_login_input_handler()
{
    rl_callback_handler_install(login_prompt.c_str(), login_input_handler);
}

void install_password_input_handler()
{
    rl_callback_handler_install(password_prompt.c_str(), password_input_handler);
    // set redisplay function for password
    rl_redisplay_function = password_redisplay;
}

void install_input_handler()
{
    state_params_t &state_params = cstate.get_params();
    (state_params.find(login_key) == state_params.end()) ?
        install_login_input_handler() :
        install_password_input_handler();
}

void process_char()
{
    signal_safe_executer executer(create_signal_mask());
    std::function<void()> func = [](){ rl_callback_read_char(); };
    executer.execute(func);
}

response_handlers_def_t get_response_handlers()
{
    return {
        // TODO (std_string) : probably add additional std::endl
        {exit_tag, [](message_response response, client_state &state){ std::cout << response.data; return EX_FINISH; }}
    };
}

}

void login_command_terminal_behavior::clear_input_action()
{
    login_command_terminal_behavior_impl::clear_input_handler();
}

void login_command_terminal_behavior::install_input_action()
{
    login_command_terminal_behavior_impl::install_input_handler();
}

void login_command_terminal_behavior::install_signal_action()
{
    login_command_terminal_behavior_impl::install_signal_handlers();
}

void login_command_terminal_behavior::process_char()
{
    login_command_terminal_behavior_impl::process_char();
}

execution_state login_command_terminal_behavior::process_server_responses(message_responses_t const &responses)
{
    response_handlers_def_t response_handlers = login_command_terminal_behavior_impl::get_response_handlers();
    return process_responses(responses, cstate, response_handlers, skip_response);
}

}