-module(blue_scribe_wifi_manager).
-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_call/3]).

-export([start_link/0, get_available_networks/0, create_new_network/2,
         get_existing_networks/0, get_networks_summary/0, delete_network/1]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_available_networks() -> [binary()].
get_available_networks() ->
    gen_server:call(?MODULE, get_available_networks).

-spec create_new_network(Ssid :: string(), Password :: string()) -> ok | {error,_}.
create_new_network(Ssid, Password) ->
    gen_server:call(?MODULE, {create, Ssid, Password}, 10000).

-spec get_existing_networks() -> [binary()].
get_existing_networks() ->
    gen_server:call(?MODULE, get_existing_networks).

-spec get_networks_summary() -> [map()].
get_networks_summary() ->
    VisibleNetworkInfo = nmcli_helper:get_available_wifi_networks(),
    AllNets = nmcli_helper:get_all_networks(),
    AllSsids = lists:map(fun(NetName) ->
                            case nmcli_helper:get_connection_ssid(NetName) of
                                {ok, Ssid} -> Ssid;
                                _ -> []
                            end
                         end, AllNets),
    AllSsids2 = lists:flatten(AllSsids), % remove []s
    Summary = lists:map(fun(Info) -> do_compose_network_summary(Info, AllSsids2) end, VisibleNetworkInfo),
    Summary.

-spec delete_network(Ssid::binary()) -> boolean().
delete_network(Ssid) ->
    case Ssid of
        <<"Hello">> ->
            true;
        _ ->
            false
    end.

init([]) ->
    case nmcli_helper:nmcli_running() of
        true ->
            {ok, #state{}};
        false ->
            {stop, "Network Manager service not running"}
    end.

handle_cast(Message, State) ->
    logger:warning("~p: Unknown cast ~p", [?MODULE, Message]),
    {noreply, State}.

handle_call(get_available_networks, _From, State) ->
    Result =
    nmcli_helper:get_available_wifi_network_ssids(),
    {reply, Result, State};

handle_call({create, Ssid, Password}, _From, State) ->
    Result = nmcli_helper:add_wifi_network(Ssid, Password),
    {reply, Result, State};

handle_call(get_existing_networks, _From, State) ->
    Result = nmcli_helper:get_all_networks(),
    {reply, Result, State};

handle_call({delete, _Network}, _From, State) ->
    {reply, not_implemented, State};

handle_call(Message, _From, _State) ->
    logger:warning("~p: Unknown call ~p", [?MODULE, Message]).


do_compose_network_summary({Ssid, Connected, Security}, VisibleSsids) ->
    Known =
    lists:member(Ssid, VisibleSsids),
    #{ssid=>Ssid, remembered=>Known, security=>Security, connected=>Connected}.





