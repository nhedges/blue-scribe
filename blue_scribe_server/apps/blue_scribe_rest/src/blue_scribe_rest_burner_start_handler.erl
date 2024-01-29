-module(blue_scribe_rest_burner_start_handler).

-export([init/2, allowed_methods/2, resource_exists/2, content_types_provided/2,
         content_types_accepted/2]).
-export([to_html/2, to_json/2]).

-record(state, {planId, powerScale}).

init(Req0, _State) ->
    {cowboy_rest, Req0, #state{}}.

allowed_methods(Req0, State) ->
    {[<<"GET">>, <<"POST">>], Req0, State}.

content_types_accepted(Req0, State) ->
    {[{{<<"multipart">>, <<"form-data">>, []}, to_html}], Req0, State}.

content_types_provided(Req0, State) ->
    Types =
    [{{<<"text">>, <<"html">>, []}, to_html}],
    {Types, Req0, State}.

resource_exists(Req0, State) ->
    {HeadersAndBody, Req1} = acc_multipart(Req0, []),
    {Headers, Bodys} = lists:unzip(HeadersAndBody),
    FormFields =
    lists:map(fun cow_multipart:form_data/1, Headers),
    ParsedForm = lists:zip(FormFields, Bodys),
    PlanIdBin = proplists:get_value({data, <<"planId">>}, ParsedForm),
    PowerScaleBin = proplists:get_value({data, <<"powerScale">>}, ParsedForm),
    {Exists, Id, PowerScale} =
    case {PlanIdBin, PowerScaleBin} of
        {undefined, _} -> 
            logger:warning("~p: No planId in request", [?MODULE]),
            {false, undefined, undefined};
        {_, undefined} ->
            logger:warning("~p: No powerScale in request", [?MODULE]),
            {false, undefined, undefined};
        {_,_} ->
            try {list_to_integer(binary_to_list(PlanIdBin)),
                 parse_float(binary_to_list(PowerScaleBin))} of
                {IdConv, PowerScaleConv} ->
                    {do_plan_exists(IdConv), IdConv, PowerScaleConv}
            catch
                _:badarg ->
                    logger:warning("~p: Badarg parsing planId: "
                                   "~p or powerScale: ~p",
                                   [?MODULE, PlanIdBin, PowerScaleBin]),
                    {false, undefined, undefined}
            end
    end,
    {Exists, Req0, State#state{planId=Id, powerScale=PowerScale}}.

do_plan_exists(PlanId) ->
    case blue_scribe_plan_db:get_plan_name(PlanId) of
        {ok,_} -> true;
        _ -> false
    end.

to_html(Req0, #state{planId=Id, powerScale=PowerScale}=State) ->
    Result=
    case do_start_burn(Id, PowerScale) of
        ok -> true;
        {error, _} -> false
    end,
    {Result, Req0, State}.

to_json(Req0, State) ->
    to_html(Req0, State).

parse_float(FloatStr) ->
    try list_to_float(FloatStr) of
        Ok -> Ok
    catch
        _:badarg ->
            list_to_float(FloatStr ++ ".0")
    end.

do_start_burn(Id, PowerScale) ->
    logger:info("~p: Got start burn; planid=~p powerScale=~p", [?MODULE, Id, PowerScale]),
    case blue_scribe_plan:load_plan(Id) of
        {error, {already_started,_}} ->
            ok;
        {ok, Id} ->
            ok;
        {error, OtherErr} ->
            logger:error("~p: ~p", [?MODULE, OtherErr])
    end,
    case blue_scribe_laser:start_burn(Id, PowerScale) of
        ok -> ok;
        {error, Err} ->
            logger:error("~p: Start burn: ~p", [?MODULE, Err]),
            {error, Err}
    end.

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
