%% @author stdstring

-module(module_name_generator).

-export([generate/2]).

%% Prefix_Ip1_Ip2_Ip3_Ip4_Port
-define(MODULE_NAME_TEMPLATE, "~p_~p_~p_~p_~p_~p").

%% ====================================================================
%% API functions
%% ====================================================================

%% TODO (std_string) : think about increasing uniqueness of module name
-spec generate(Prefix :: atom(), SocketOtherSide :: {Address :: tuple(), Port :: pos_integer()}) -> atom().
generate(Prefix, {{Ip1, Ip2, Ip3, Ip4}, Port}) ->
    list_to_atom(string_utils:format(?MODULE_NAME_TEMPLATE, [Prefix, Ip1, Ip2, Ip3, Ip4, Port])).

%% ====================================================================
%% Internal functions
%% ====================================================================