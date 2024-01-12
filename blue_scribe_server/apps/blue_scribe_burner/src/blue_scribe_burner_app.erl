%%%-------------------------------------------------------------------
%% @doc blue_scribe_burner public API
%% @end
%%%-------------------------------------------------------------------

-module(blue_scribe_burner_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    blue_scribe_burner_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
