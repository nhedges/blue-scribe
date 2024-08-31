-module(blue_scribe_rest_wifi_handler).

-export([init/2, allowed_methods/2, resource_exists/2, content_types_provided/2,
         content_types_accepted/2, delete_resource/2, to_html/2, edit_config/2]).

-export([to_json/2]).

-export([do_ssid_in_config/1]).


init(Req0, State) ->
    {cowboy_rest, Req0, State}.

allowed_methods(Req0, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req0, State}.

content_types_accepted(Req0, State) ->
    {[{{<<"multipart">>, <<"form-data">>, []}, edit_config}], Req0, State}.

content_types_provided(Req0, State) ->
    Types =
    [
     {{<<"text">>, <<"html">>, []}, to_html},
     {{<<"application">>, <<"json">>, []}, to_json}
    ],
    {Types, Req0, State}.


resource_exists(Req0, State) ->
    case cowboy_req:path_info(Req0) of
        [<<"networks">>] -> % all available and remembered networks, with metadata
            {true, Req0, State};
        [<<"connect">>] ->
            {true, Req0, State};
        [<<"candidates">>] -> % a list of visible networks
            {true, Req0, State};
        [] ->
            % shouldn't be possible, should hit static file rule
            {false, Req0, State};
        [<<"known">>] -> % a list of stored networks
            {true, Req0, State}
    end.

    %logger:info("~p: ~p", [?MODULE, Req0]),
    %{HeadersAndBody, Req1} = acc_multipart(Req0, []),
    %{Headers, Bodys} = lists:unzip(HeadersAndBody),
    %FormFields =
    %lists:map(fun cow_multipart:form_data/1, Headers),
    %ParsedForm = lists:zip(FormFields, Bodys),
    %Ssid = proplists:get_value({data, <<"ssid">>}, ParsedForm),
    %SsidInConfig = do_ssid_in_config(Ssid),
    %{SsidInConfig, Req0, State}.

delete_resource(Req0, State) ->
    case cowboy_req:qs(Req0) of
        <<"ssid=",IdStr/binary>> ->
            Result = blue_scribe_wifi_manager:delete_network(IdStr),
            {Result, Req0, State}
    end.

edit_config(Req0, State) ->
    {HeadersAndBody, Req1} = acc_multipart(Req0, []),
    {Headers, Bodys} = lists:unzip(HeadersAndBody),
    FormFields =
    lists:map(fun cow_multipart:form_data/1, Headers),
    ParsedForm = lists:zip(FormFields, Bodys),
    SsidBin = proplists:get_value({data, <<"ssid">>}, ParsedForm),
    PskBin = proplists:get_value({data, <<"psk">>}, ParsedForm),
    blue_scribe_wifi_manager:create_new_network(binary_to_list(SsidBin), binary_to_list(PskBin)),
    {true, Req0, State}.

to_html(Req0, State) ->
    {<<>>, Req0, State}.

to_json(Req0, State) ->
    case cowboy_req:path_info(Req0) of
        [<<"networks">>] ->
            Result = jiffy:encode(blue_scribe_wifi_manager:get_networks_summary()),
            %AvailableList = blue_scribe_wifi_manager:get_available_networks(),
            %RememberedList = blue_scribe_wifi_manager:get_existing_networks(),
            %ConnectionMapList =
            %lists:map(fun(X) ->
            %  Remembered = lists:member(X, RememberedList),
            %  Security = <<"Unknown">>,
            %  Connected = false,
            %  #{ssid=>X, remembered=>Remembered, security=>Security, connected=>Connected}
            %end,
            %AvailableList),
            %Result = jiffy:encode(ConnectionMapList),
            {Result, Req0, State};

        [<<"candidates">>] ->
            BinList =
            blue_scribe_wifi_manager:get_available_networks(),
            Result = jiffy:encode(BinList),
            {Result, Req0, State};
        [<<"known">>] ->
            BinList =
            blue_scribe_wifi_manager:get_existing_networks(),
            Result = jiffy:encode(BinList),
            {Result, Req0, State}
    end.

-spec do_ssid_in_config(Ssid :: string() | binary()) -> boolean().
do_ssid_in_config(Ssid) when is_list(Ssid) ->
    do_ssid_in_config(list_to_binary(Ssid));
do_ssid_in_config(Ssid) ->
    Networks = nmcli_helper:get_all_networks(),
    AllSsids =
    lists:map(fun(NetName) ->
                      case nmcli_helper:get_connection_ssid(NetName) of
                          {ok, Answer} -> Answer;
                          {error, _} -> <<>>
                      end
              end, Networks),
    lists:member(Ssid, AllSsids).

acc_multipart(Req0, Acc) ->
    case cowboy_req:read_part(Req0) of
        {ok, Headers, Req1} ->
            {ok, Body, Req} = stream_body(Req1, <<>>),
            acc_multipart(Req, [{Headers, Body}|Acc]);
        {done, Req} ->
            {lists:reverse(Acc), Req}
    end.

stream_body(Req0, Acc) ->
    case cowboy_req:read_part_body(Req0) of
        {more, Data, Req} ->
            stream_body(Req, << Acc/binary, Data/binary >>);
        {ok, Data, Req} ->
            {ok, << Acc/binary, Data/binary >>, Req}
    end.
