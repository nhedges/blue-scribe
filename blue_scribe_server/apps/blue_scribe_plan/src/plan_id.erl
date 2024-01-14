-module(plan_id).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).%, handle_info/2]).

-export([start_link/0, get_id/0]).

-record(state, {next_id=0 :: non_neg_integer()}).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_id() ->
    gen_server:call(?SERVER, get_id).

init([]) ->
    {ok, #state{}}.

handle_call(get_id, _, #state{next_id=Next}=State) ->
    {reply, Next, State#state{next_id=Next+1}};

handle_call(Call, _, State) ->
    logger:warning("~p: Unknown call ~p", [?MODULE, Call]),
    {reply, ok, State}.

handle_cast(Cast, State) ->
    logger:warning("~p: Unknown call ~p", [?MODULE, Cast]),
    {noreply, State}.
