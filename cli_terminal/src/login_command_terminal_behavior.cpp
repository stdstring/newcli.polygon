#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>

#include <readline.h>
#include "signal.h"

#include "base64.h"
#include "cli_io_helper.h"
#include "client_state.h"
#include "cterm_ptr.h"
#include "input_terminal_behavior.h"
#include "iterminal_behavior.h"
#include "login_command_terminal_behavior.h"
#include "signal_utils.h"
#include "string_utils.h"

#define LOGIN_KEY "login"
#define LOGIN_PROMPT "login:"
#define PASSWORD_PROMPT "password:"
#define LOGIN_COMMAND_PREFIX "login"

namespace cli_terminal
{

extern client_state cstate;

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
    state_params.erase(LOGIN_KEY);
    // input terminal behavior
    set_behavior(cstate, std::shared_ptr<iterminal_behavior>(new input_terminal_behavior()));
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

std::string create_command(std::string const &login, std::string const &password)
{
    std::string command(LOGIN_COMMAND_PREFIX);
    command.push_back(' ');
    command.append(login);
    command.push_back(' ');
    std::string password_base64 = to_base64(password);
    command.append(password_base64);
    return command;
}

void login_input_handler(char *raw_data)
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
    state_params.emplace(LOGIN_KEY, login);
    // install password_input_handler
    clear_input_handler();
    install_password_input_handler();
}

void password_input_handler(char *raw_data)
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
    std::string login = state_params.at(LOGIN_KEY);
    state_params.erase(LOGIN_KEY);
    // form login command
    std::string login_command = create_command(login, password);
    // execute login command
    execution_state ex_state = process_request(login_command, cstate);
    cstate.set_execution_state(ex_state);
}

void clear_input_handler()
{
    rl_callback_handler_remove();
}

void install_login_input_handler()
{
    rl_callback_handler_install(LOGIN_PROMPT, login_input_handler);
}

void install_password_input_handler()
{
    rl_callback_handler_install(PASSWORD_PROMPT, password_input_handler);
}

void install_input_handler()
{
    state_params_t &state_params = cstate.get_params();
    (state_params.find(LOGIN_KEY) == state_params.end()) ?
        install_login_input_handler() :
        install_password_input_handler();
}

void process_char()
{
    rl_callback_read_char();
}

}

void login_command_terminal_behavior::install_input_action()
{
    login_command_terminal_behavior_impl::clear_input_handler();
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

}