%%%-------------------------------------------------------------------
%% @doc blue_scribe_plan public API
%% @end
%%%-------------------------------------------------------------------

-module(blue_scribe_plan_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mnesia:wait_for_tables([blue_scribe_plan], 5000),
    blue_scribe_plan_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
