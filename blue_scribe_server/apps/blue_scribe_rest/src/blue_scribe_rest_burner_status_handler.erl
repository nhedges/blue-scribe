-module(blue_scribe_rest_burner_status_handler).

-export([init/2, allowed_methods/2, resource_exists/2, content_types_provided/2,
         content_types_accepted/2]).
-export([to_html/2, to_json/2, modify_status/2]).

init(Req0, State) ->
    {cowboy_rest, Req0, State}.

allowed_methods(Req0, State) ->
    {[<<"GET">>, <<"POST">>], Req0, State}.

content_types_accepted(Req0, State) ->
    {[%{{<<"text">>, <<"html">>, []}, to_html},
      {{<<"multipart">>, <<"form-data">>, '*'}, modify_status}], Req0, State}.

content_types_provided(Req0, State) ->
    Types =
    [%{{<<"text">>, <<"html">>, []}, to_html},
     {{<<"application">>, <<"json">>, []}, to_json}],
    {Types, Req0, State}.

resource_exists(Req0, State) ->
    %{HeadersAndBody, Req1} = acc_multipart(Req0, []),
    %{Headers, Bodys} = lists:unzip(HeadersAndBody),
    %FormFields =
    %lists:map(fun cow_multipart:form_data/1, Headers),
    %ParsedForm = lists:zip(FormFields, Bodys),
    {true, Req0, State}.

    %case cowboy_req:method(Req0) of
    %    <<"GET">> ->
    %        {true, Req0, State};
    %    <<"PATCH">> ->
    %        {true, Req0, State};
    %    _ ->
    %        {false, Req0, State}
    %end.

to_html(Req0, State) ->
    {<<>>, Req0, State}.

to_json(Req0, State) ->
    case blue_scribe_laser:status() of
        {ok, Status} ->
            StatusJson = jiffy:encode(list_to_tuple(do_status_stringmap(Status))),
            {StatusJson, Req0, State};
        {error, Err} ->
            logger:error("~p: ~p", [?MODULE, Err]),
            {stop, Req0, State}
    end.

modify_status(Req0, State) ->
    {HeadersAndBody, Req1} = acc_multipart(Req0, []),
    {Headers, Bodys} = lists:unzip(HeadersAndBody),
    FormFields =
    lists:map(fun cow_multipart:form_data/1, Headers),
    ParsedForm = lists:zip(FormFields, Bodys),
    lists:foreach(
      fun({{data, <<"paused">>}, <<"true">>}) ->
              blue_scribe_laser:pause_burn();
         ({{data, <<"paused">>}, <<"false">>}) ->
              blue_scribe_laser:resume_burn();
         ({{data, <<"home">>}, <<"true">>}) ->
              blue_scribe_laser:home();
         ({{data, <<"cancel">>}, <<"true">>}) ->
              blue_scribe_laser:cancel_burn();
         (Other) ->
              logger:warning("~p: Unknown field ~p", [?MODULE, Other])
      end,
      ParsedForm),
    {true, Req0, State}.



do_status_stringmap(Status) ->
    [[{list_to_binary(atom_to_list(X)), Y} || {X, Y} <- Status]].

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
