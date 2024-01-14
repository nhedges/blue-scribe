%%%-------------------------------------------------------------------
%% @doc blue_scribe_plan top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(blue_scribe_plan_sup).

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
                 intensity => 0,
                 period => 1},
    ServerSup =
    #{id => server_sup,
      start => {blue_scribe_plan_server_sup, start_link, []},
      restart => transient,
      shutdown => 300,
      type => supervisor,
      modules => [blue_scribe_plan_server_sup]},
    PlanId =
    #{id => plan_id,
      start => {plan_id, start_link, []},
      restart => transient,
      shutdown => 10,
      type => worker,
      modules => [plan_id]},
    ChildSpecs = [PlanId, ServerSup],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
