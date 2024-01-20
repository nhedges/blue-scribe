%%%-------------------------------------------------------------------
%% @doc blue_scribe_rest public API
%% @end
%%%-------------------------------------------------------------------

-module(blue_scribe_rest_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile(
                 [{'_', [{"/plan/[...]", blue_scribe_rest_plan_handler, []},
                         {"/[...]", cowboy_static, {priv_dir,
                                                    blue_scribe_rest,
                                                    "",
                                                    [
                                                     {mimetypes, cow_mimetypes, all},
                                                     {dir_handler, directory_h},
                                                     {charset, <<"utf-8">>}]}}
                         %{"/", hello_handler, []},
                         ]}]),
    {ok, _} = cowboy:start_clear(my_http_listener,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch}}),
    blue_scribe_rest_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
