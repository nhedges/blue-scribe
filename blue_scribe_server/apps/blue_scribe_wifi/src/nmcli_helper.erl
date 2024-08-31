-module(nmcli_helper).

-export([nmcli_running/0, connected/0, get_all_networks/0,
         get_connection_ssid/1, connection_autoconnects/1,
         add_wifi_network/2, get_available_wifi_network_ssids/0,
         get_available_wifi_networks/0,
         make_ad_hoc_network/2, disable_ad_hoc_network/0,
         forget_network/1]).

-spec nmcli_running() -> boolean().
nmcli_running() ->
    case os:cmd("nmcli -t -f RUNNING general") of
        "running\n" -> true;
        _ -> false
    end.

-spec connected() -> boolean().
connected() ->
    case os:cmd("nmcli -t -f STATE general") of
        "connected\n" -> true;
        _ -> false
    end.

-spec get_all_networks() -> [NetworkName :: binary()].
get_all_networks() ->
    OutputStr = os:cmd("nmcli -f Name c"),
    [_ | NetworksBinList] = re:split(OutputStr, " *\n"),
    NetworksBinList.

-spec get_connection_ssid(ConnectionName :: string() | binary()) ->
    {ok, binary()} | {error, _}.
get_connection_ssid(Name) when is_binary(Name) ->
    get_connection_ssid(binary_to_list(Name));
get_connection_ssid(ConnectionName) ->
    OutputStr = os:cmd("nmcli -f 802-11-wireless.ssid -t c show '" ++ ConnectionName ++ "'"),
    do_get_field("802-11-wireless.ssid:", OutputStr).

-spec connection_autoconnects(ConnectionName :: string() | binary()) ->
    {ok, boolean()} | {error, _}.
connection_autoconnects(Name) when is_binary(Name) ->
    connection_autoconnects(binary_to_list(Name));
connection_autoconnects(ConnectionName) ->
    OutputStr = os:cmd("nmcli -f connection.autoconnect -t c show '" ++ ConnectionName ++ "'"),
    case do_get_field("connection.autoconnect:", OutputStr) of
        {ok, <<"yes">>} -> {ok, true};
        {ok, <<"no">>} -> {ok, false};
        {error, Err} -> {error, Err}
    end.

-spec add_wifi_network(Ssid :: string(), Password :: string()) -> ok | {error,_}.
add_wifi_network(Ssid, Password) ->
    Output =
    case Password of
        "" ->
            os:cmd("nmcli device wifi connect '" ++ Ssid ++ "'");
        _ ->
            os:cmd("nmcli device wifi connect '" ++ Ssid ++ "' password '" ++ Password ++ "'")
    end,
    case re:run(Output, "[Ee][Rr][Rr][Oo][Rr]") of
        nomatch -> ok;
        _ -> {error, Output}
    end.

-spec get_available_wifi_network_ssids() -> [binary()].
get_available_wifi_network_ssids() ->
    Output = os:cmd("nmcli -f SSID -t dev wifi"),
    [ X || X <- re:split(Output, "\n"), X =/= <<>>].

-spec get_available_wifi_networks() -> [{binary(), boolean(), binary()}].
get_available_wifi_networks() ->
    % command provides string where SSID, IN-USE, and SECURITY are separated by colons (:)
    % and networks are separated by newlines
    Output = os:cmd("nmcli -f SSID,IN-USE,SECURITY -t dev wifi"),
    % use regex to separate networks
    Pairs = re:split(Output, "\n"),
    TupleList =
    lists:map(fun(Pair) ->
                % use regex to separate fields
                case re:split(Pair, ":") of
                    [<<>>,_, _] -> error;
                    [_, <<>>, _] -> error;
                    [_, _, <<>>] -> error;
                    [Ssid, <<" ">>, Sec] -> {Ssid, false, Sec};
                    [Ssid, <<"*">>, Sec] -> {Ssid, true, Sec};
                    _Other -> error
                end
              end,
              Pairs),
    % remove errors from the list
    lists:filter(fun(X) -> X =/= error end, TupleList).

-spec make_ad_hoc_network(Ssid :: string(), Password :: string()) -> ok | {error, _}.
make_ad_hoc_network(Ssid, Password) ->
    Output = os:cmd("nmcli dev wifi hotspot con-name Hotspot ssid '" ++ Ssid ++ "' password '" ++ Password ++ "'"),
    case re:run(Output, "[Ee][Rr][Rr][Oo][Rr]") of
        nomatch -> ok;
        _ -> {error, Output}
    end.

-spec disable_ad_hoc_network() -> ok | {error,_}.
disable_ad_hoc_network() ->
    Output = os:cmd("nmcli connection down Hotspot"),
    case re:run(Output, "[Ee][Rr][Rr][Oo][Rr]") of
        nomatch -> ok;
        _ -> {error, Output}
    end.

-spec do_get_field(FieldName :: string(), OutputStr :: string()) ->
    {ok, binary()} | {error,_}.
do_get_field(FieldName, OutputStr) ->
    case re:split(OutputStr, FieldName) of
        [<<>>, ResultWithNewline] ->
            [Result | _] = re:split(ResultWithNewline, "\n"),
            {ok, Result};
        _ ->
            {error, not_found}
    end.

forget_network(Ssid) when is_binary(Ssid) ->
    forget_network(binary_to_list(Ssid));

forget_network(Ssid) ->
    Output = os:cmd("nmcli connection delete " ++ Ssid),
    case re:run(Output, "[Ee][Rr][Rr][Oo][Rr]") of
        nomatch -> ok;
        _ -> {error, Output}
    end.
