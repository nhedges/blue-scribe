-module(blue_scribe_rest_plan_handler).

-export([init/2, allowed_methods/2, content_types_provided/2,
         content_types_accepted/2]).
-export([plan_text/2, plan_html/2, plan_json/2, plan_png/2, create_plan/2]).


init(Req0, State) ->
    {cowboy_rest, Req0, State}.


allowed_methods(Req0, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req0, State}.

content_types_provided(Req0, State) ->
    Types = case cowboy_req:path_info(Req0) of
                [Id, <<"preview.png">>] ->
                    [{{<<"image">>, <<"png">>, []}, plan_png}];
                _ ->
                    [
                     {{<<"text">>, <<"html">>, []}, plan_html},
                     {{<<"application">>, <<"json">>, []}, plan_json}
                    ]
            end,
    {Types, Req0, State}.

content_types_accepted(Req0, State) ->
    {[{{<<"multipart">>, <<"form-data">>, []}, create_plan}], Req0, State}.


plan_text(Req0, State) ->
    <<"id=", IdBin/binary>> = cowboy_req:qs(Req0),
    Id = list_to_integer(binary_to_list(IdBin)),
    {ok, Name} = blue_scribe_plan_db:get_plan_name(Id),
    {ok, Desc} = blue_scribe_plan_db:get_plan_notes(Id),
    Res = "Plan name: " ++ Name ++ "\n"
    "Plan notes: " ++ Desc ++ "\n",
    {list_to_binary(Res), Req0, State}.

plan_html(Req0, State) ->
    Res =
    case cowboy_req:qs(Req0) of
        <<>> ->
            plan_png(Req0, State);
        <<"id=", IdBin/binary>> ->
            Id = list_to_integer(binary_to_list(IdBin)),
            {ok, Name} = blue_scribe_plan_db:get_plan_name(Id),
            {ok, Desc} = blue_scribe_plan_db:get_plan_notes(Id),
            Str =
            "<p> Plan name: " ++ Name ++ "</p>"
            "<p> Plan desc: " ++ Desc ++ "</p>",
            list_to_binary(Str);
        <<"all">> ->
            Ids = blue_scribe_plan_db:get_all_plan_ids(),
            Plans =
            lists:map(fun(Id) ->
                              {ok, Name} = blue_scribe_plan_db:get_plan_name(Id),
                              {ok, Desc} = blue_scribe_plan_db:get_plan_notes(Id),
                              "<p> Plan name: " ++ Name ++ "</p>"
                              "<p> Plan desc: " ++ Desc ++ "</p>"
                      end,
                      Ids),
            list_to_binary(lists:flatten(Plans))
    end,
    {Res, Req0, State}.

plan_json(Req0, State) ->
    Res =
    case cowboy_req:qs(Req0) of
        <<"some">> ->
            Ids = blue_scribe_plan_db:select_popular_plans(20),
            Plans = lists:map(fun do_json_ready_plan/1, Ids),
            jiffy:encode(Plans);
        <<"all">> ->
            Ids = blue_scribe_plan_db:get_all_plan_ids(),
            Plans =
            lists:map(fun do_json_ready_plan/1, lists:sort(Ids)),
            jiffy:encode(Plans);
        <<"id=", IdBin/binary>> ->
            Id = list_to_integer(binary_to_list(IdBin)),
            jiffy:encode(do_json_ready_plan(Id))
    end,
    {Res, Req0, State}.

do_json_ready_plan(PlanId) ->
    {ok, Name} = blue_scribe_plan_db:get_plan_name(PlanId),
    {ok, Desc} = blue_scribe_plan_db:get_plan_notes(PlanId),
    {ok, {Width, Height}} = blue_scribe_plan_db:get_plan_dimensions(PlanId),
    {[{<<"id">>, PlanId},
      {<<"name">>, list_to_binary(Name)},
      {<<"desc">>, list_to_binary(Desc)},
      {<<"width">>, integer_to_binary(Width)},
      {<<"height">>, integer_to_binary(Height)}]}.

plan_png(Req0, State) ->
    Res =
    case cowboy_req:path_info(Req0) of
        [PlanIdBin, <<"preview.png">>] ->
            case blue_scribe_plan_db:get_plan_preview_png(
                   list_to_integer(binary_to_list(PlanIdBin))) of
                {ok, Bin} ->
                    Bin;
                {error, not_found} ->
                    <<>>
            end;
        _ ->
            <<>>
    end,
    {Res, Req0, State}.

create_plan(Req0, State) ->
    {HeadersAndBody, Req1} = acc_multipart(Req0, []),
    {Headers, Bodys} = lists:unzip(HeadersAndBody),
    FormFields =
    lists:map(fun cow_multipart:form_data/1, Headers),
    ParsedForm = lists:zip(FormFields, Bodys),
    NameBin = proplists:get_value({data, <<"name">>}, ParsedForm),
    DescBin = proplists:get_value({data, <<"desc">>}, ParsedForm),
    {value,{FileInfo,FileBin}} =
    lists:search(fun({{file, <<"image">>, _Filename, _FileType},_Data}) -> true;
                    (_) -> false
                 end,
                 ParsedForm),
    CreateRes =
    blue_scribe_plan_db:create_plan(FileBin,
                                    binary_to_list(NameBin),
                                    binary_to_list(DescBin)),
    case CreateRes of
        {ok, NewPlanId} ->
            logger:notice("~p: Created new plan with id ~p", [?MODULE, NewPlanId]),
            {true, Req1, State};
        {error, Err} ->
            logger:warning("~p: Plan not created because ~p", [?MODULE, Err]),
            {false, Req1, State}
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
