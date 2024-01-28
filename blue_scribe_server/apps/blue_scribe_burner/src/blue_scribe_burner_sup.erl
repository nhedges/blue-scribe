%%%-------------------------------------------------------------------
%% @doc blue_scribe_burner top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(blue_scribe_burner_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 5,
                 period => 5},
    BlueScribeLaser =
    #{id => blue_scribe_laser,
      start => {blue_scribe_laser, start_link, []},
      restart => transient,
      shutdown => 100,
      type => worker,
      modules => [blue_scribe_laser]},
    ChildSpecs = [BlueScribeLaser],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
