-module(blue_scribe_plan).
-behaviour(gen_server).

-export([load_plan/1, get_plan/1, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2]).%, handle_info/2]).

-include_lib("blue_scribe_burner/include/blue_scribe_burner.hrl").

-define(CROP_X, 380).
-define(CROP_Y, 380).

-record(state,
        {plan}).

-spec load_plan(Id :: non_neg_integer()) -> non_neg_integer() | {error,_}.
load_plan(Id) ->
    case blue_scribe_plan_server_sup:start_child(Id) of
        {ok, _Pid} -> {ok, Id};
        Other -> Other
    end.

-spec get_plan(PlanId :: non_neg_integer()) ->
    {ok, _}|{error,_}.
get_plan(Id) ->
    gen_server:call({global,{?MODULE, Id}}, get_plan).

start_link(Id) ->
    gen_server:start_link({global, {?MODULE, Id}}, ?MODULE, [Id], []).

init([Id]) ->
    Filename = blue_scribe_plan_db:get_png_filename(Id),
    Img = blue_scribe_plan_image:do_load_png_file(Filename),
    ImgCropped = blue_scribe_plan_image:do_crop_image(Img,?CROP_Y, ?CROP_X),
    Plan =
    case blue_scribe_plan_db:get_plan_op_list(Id) of
        {error, no_plan} ->
            NewPlan =
            blue_scribe_plan_image:do_image_to_plan(ImgCropped),
            blue_scribe_plan_db:update_plan_op_list(Id, NewPlan),
            NewPlan;
        {ok, CachedPlan} ->
            CachedPlan;
        {error, not_found} ->
            logger:error("~p: Error: plan ~p not found", [?MODULE, Id]),
            blue_scribe_plan_image:do_image_to_plan(ImgCropped)
    end,
    {ok, #state{plan=Plan}}.

handle_call(get_plan, _From, #state{plan=Plan}=State) ->
    {reply, {ok, Plan}, State};

handle_call(Call, _, State) ->
    logger:warning("~p: Unknown call ~p", [?MODULE, Call]),
    {reply, ok, State}.

handle_cast(Cast, State) ->
    logger:warning("~p: Unknown call ~p", [?MODULE, Cast]),
    {noreply, State}.



