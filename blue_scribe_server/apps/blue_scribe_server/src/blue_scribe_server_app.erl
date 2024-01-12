%%%-------------------------------------------------------------------
%% @doc blue_scribe_server public API
%% @end
%%%-------------------------------------------------------------------

-module(blue_scribe_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    blue_scribe_server_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
