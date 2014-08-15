-module(prompt_factory).

-include("common_defs.hrl").
-include("logic_utils_defs.hrl").

-define(ADMIN_FOOTER, "#").
-define(USER_FOOTER, ">").
-define(PROMPT_FORMAT, "~s@~s~s~s").

-export([generate_prompt/1]).

-spec generate_prompt(ExecutionState :: #execution_state{}) -> string().
generate_prompt(ExecutionState) ->
    LoginInfo = ExecutionState#execution_state.login_info,
    LoginPart = logic_utils:ternary_lazy_op(LoginInfo == undefined, ?LAZY(""), ?LAZY(LoginInfo#login_info.login_name)),
    DeviceName = ExecutionState#execution_state.device_name,
    CliMode = ExecutionState#execution_state.current_cli_mode,
    CliModePart = logic_utils:ternary_lazy_op((CliMode == undefined) or (CliMode == ""), ?LAZY(""), ?LAZY(" (" ++ CliMode ++ ")")),
    IsAdmin = logic_utils:ternary_lazy_op(LoginInfo == undefined, ?LAZY(false), ?LAZY(LoginInfo#login_info.is_admin)),
    PromptFooter = logic_utils:ternary_op(IsAdmin, ?ADMIN_FOOTER, ?USER_FOOTER),
    Prompt = io_lib:format(?PROMPT_FORMAT, [LoginPart, DeviceName, CliModePart, PromptFooter]),
    lists:flatten(Prompt).