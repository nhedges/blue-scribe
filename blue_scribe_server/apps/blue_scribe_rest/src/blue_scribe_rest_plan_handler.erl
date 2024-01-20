-module(blue_scribe_rest_plan_handler).

-export([init/2]).

-define(CT, <<"content-type">>).
-define(TXT, <<"text/plain">>).
-define(PNG, <<"image/png">>).
-define(JSON, <<"application/json">>).
-define(GET, <<"GET">>).
-define(POST, <<"POST">>).
-define(DEL, <<"DELETE">>).

init(Req0, State) ->
    Req =
    case {cowboy_req:method(Req0), cowboy_req:path_info(Req0)} of
        {?GET, [<<"getAll">>]} ->
            AllPlanIds = blue_scribe_plan_db:get_all_plan_ids(),
            cowboy_req:reply(200,
                             #{?CT => ?JSON},
                             jiffy:encode(AllPlanIds),
                             %erl_to_formatted_bin(AllPlanIds),
            Req0);
        {?GET, [PlanIdBin, <<"name">>]} ->
            case blue_scribe_plan_db:get_plan_name(
                   list_to_integer(binary_to_list(PlanIdBin))) of
                {ok, Name} ->
                    cowboy_req:reply(200,
                                     #{?CT => ?TXT},
                                     Name,
                                     Req0);
                {error, not_found} ->
                    cowboy_req:reply(404, Req0)
            end;
        {?GET, [PlanIdBin, <<"notes">>]} ->
            case blue_scribe_plan_db:get_plan_notes(
                   list_to_integer(binary_to_list(PlanIdBin))) of
                {ok, Notes} ->
                    cowboy_req:reply(200,
                                     #{?CT => ?TXT},
                                     Notes,
                                     Req0);
                {error, not_found} ->
                    cowboy_req:reply(404, Req0)
            end;
        {?GET, [PlanIdBin, <<"preview.png">>]} ->
            case blue_scribe_plan_db:get_plan_preview_png(
                   list_to_integer(binary_to_list(PlanIdBin))) of
                {ok, Bin} ->
                    cowboy_req:reply(200,
                                     #{?CT => ?PNG},
                                     Bin,
                                     Req0);
                {error, not_found} ->
                    cowboy_req:reply(404, Req0)
            end;
        {?POST, [<<"new">>]} ->
            %TODO handle multi-part segmentation
            {ok, Headers, Req2} = cowboy_req:read_part(Req0),
            {ok, Data, Req3} = cowboy_req:read_part_body(Req2),
            {file, _, Filename, ContentType}
            = cow_multipart:form_data(Headers),
            case blue_scribe_plan_db:create_plan(Data, Filename, "") of
                {ok, NewPlanId} ->
                    cowboy_req:reply(200,
                                     #{?CT => ?TXT},
                                     erl_to_formatted_bin(NewPlanId),
                                     Req0);
                {error, Err} ->
                cowboy_req:reply(400, Req0)
            end;
        {?DEL, [PlanIdBin]} ->
            case blue_scribe_plan_db:delete_plan(
                   list_to_integer(binary_to_list(PlanIdBin))) of
                ok ->
                    cowboy_req:reply(200, Req0);
                {error, not_found} ->
                    cowboy_req:reply(404, Req0)
            end;
        Other ->
            logger:warning("~p: Unknown path: ~p", [?MODULE, Other]),
            cowboy_req:reply(400, Req0)
    end,
    {ok, Req, State}.

init_file(Req, Opts) ->
	{ok, Headers, Req2} = cowboy_req:read_part(Req),
	{ok, Data, Req3} = cowboy_req:read_part_body(Req2),
	{file, _, Filename, ContentType}
		= cow_multipart:form_data(Headers),
	io:format("Received file ~p of content-type ~p as follow:~n~p~n~n",
		[Filename, ContentType, Data]),
	{ok, Req3, Opts}.

erl_to_formatted_bin(Term) ->
    list_to_binary(
      lists:flatten(
        io_lib:format("~p", [Term]))).

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
