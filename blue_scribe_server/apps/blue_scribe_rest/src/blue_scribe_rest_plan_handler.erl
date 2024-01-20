-module(blue_scribe_rest_plan_handler).

-export([init/2, allowed_methods/2, content_types_provided/2,
         content_types_accepted/2]).
-export([plan_text/2, plan_html/2, plan_json/2, plan_png/2]).


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
            logger:warning("~p: plan_html(~p, ~p)", [?MODULE, Req0, Id]),
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
    logger:notice("~p: plan_json ~p", [?MODULE, Req0]),
    Res =
    case cowboy_req:qs(Req0) of
        <<"all">> ->
            Ids = blue_scribe_plan_db:get_all_plan_ids(),
            Plans =
            lists:map(fun(Id) ->
                              {ok, Name} = blue_scribe_plan_db:get_plan_name(Id),
                              {ok, Desc} = blue_scribe_plan_db:get_plan_notes(Id),
                              {[{<<"id">>, Id},
                                {<<"name">>, list_to_binary(Name)},
                                {<<"desc">>, list_to_binary(Desc)}]}
                      end,
                      Ids),
            jiffy:encode(Plans);
        <<"id=", IdBin/binary>> ->
            Id = list_to_integer(binary_to_list(IdBin)),
            logger:warning("~p: plan_json(~p, ~p)", [?MODULE, Req0, Id]),
            {ok, Name} = blue_scribe_plan_db:get_plan_name(Id),
            {ok, Desc} = blue_scribe_plan_db:get_plan_notes(Id),
            jiffy:encode({[{<<"id">>, Id},
                           {<<"name">>, list_to_binary(Name)},
                           {<<"desc">>, list_to_binary(Desc)}]})
    end,
    {Res, Req0, State}.

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

