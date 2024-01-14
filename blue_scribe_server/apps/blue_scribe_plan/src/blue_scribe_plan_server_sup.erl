%%%-------------------------------------------------------------------
%% @doc blue_scribe_plan top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(blue_scribe_plan_server_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_child/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Id, File) ->
    io:format("Start child ~n"),
    supervisor:start_child(?SERVER, [Id, File]).

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
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 3,
                 period => 10},
    PlanServer =
    #{id=>plan_server,
      start => {blue_scribe_plan, start_link, []},
      restart => transient,
      shutdown => 100,
      type => worker,
      modules => [blue_scribe_plan]},
    ChildSpecs = [PlanServer],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
