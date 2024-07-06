%%%-------------------------------------------------------------------
%% @doc blue_scribe_wifi public API
%% @end
%%%-------------------------------------------------------------------

-module(blue_scribe_wifi_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    blue_scribe_wifi_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
