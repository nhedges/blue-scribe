-module(blue_scribe_plan).
-behaviour(gen_server).

-export([new_plan/1, get_plan/1, start_link/2]).

-export([init/1, handle_call/3, handle_cast/2]).%, handle_info/2]).

-include_lib("blue_scribe_burner/include/blue_scribe_burner.hrl").

-define(CROP_X, 380).
-define(CROP_Y, 380).

-record(state,
        {plan}).

-spec new_plan(Filename :: string()) -> non_neg_integer() | {error,_}.
new_plan(Filename) ->
    Id = plan_id:get_id(),
    {ok, _} = blue_scribe_plan_server_sup:start_child(Id, Filename),
    {ok, Id}.

-spec get_plan(PlanId :: non_neg_integer()) ->
    {ok, _}|{error,_}.
get_plan(Id) ->
    gen_server:call({global,{?MODULE, Id}}, get_plan).

start_link(Id, Filename) ->
    gen_server:start_link({global, {?MODULE, Id}}, ?MODULE, [Id, Filename], []).

init([_Id, Filename]) ->
    Img = blue_scribe_plan_image:do_load_png_file(Filename),
    ImgCropped = blue_scribe_plan_image:do_crop_image(Img,?CROP_Y, ?CROP_X),
    Plan = blue_scribe_plan_image:do_image_to_plan(ImgCropped, 2.0),
    %TODO specify power scale instead of hardcode 2.0
    {ok, #state{plan=Plan}}.

handle_call(get_plan, _From, #state{plan=Plan}=State) ->
    {reply, {ok, Plan}, State};

handle_call(Call, _, State) ->
    logger:warning("~p: Unknown call ~p", [?MODULE, Call]),
    {reply, ok, State}.

handle_cast(Cast, State) ->
    logger:warning("~p: Unknown call ~p", [?MODULE, Cast]),
    {noreply, State}.



