-module(blue_scribe_laser).
-behaviour(gen_server).

-export([start_link/0, start_burn/1, home/0, corner_align_next/0, pause_burn/0,
         resume_burn/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%TEMP
-export([do_setup_serial/3]).

-include_lib("blue_scribe_burner/include/blue_scribe_burner.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.%TEST

-record(state,{serial_pid :: pid() | undefined,
               plan :: laser_plan() | corner_alignment | undefined,
               active_op :: laser_cmdop() | undefined,
               corner_alignment :: 0..3 | undefined,
               paused = false :: boolean()}).

-spec start_burn(PlanId :: non_neg_integer()) -> ok | {error,_}.
start_burn(PlanId) ->
    gen_server:call(?MODULE, {start_burn, PlanId}).

-spec home() -> ok | {error,_}.
home() ->
    gen_server:call(?MODULE, home).

-spec corner_align_next() -> ok | {error,_}.
corner_align_next() ->
    gen_server:call(?MODULE, corner).

-spec pause_burn() -> ok | {error, _}.
pause_burn() ->
    gen_server:call(?MODULE, pause).

-spec resume_burn() -> ok | {error, _}.
resume_burn() ->
    gen_server:call(?MODULE, resume).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, SerialPid} = laser_serial_mockup:start(self()),
    {ok, #state{serial_pid=SerialPid}}.

handle_call(_, _From, #state{serial_pid=undefined}=State) ->
    {reply, {error, no_serial}, State};

handle_call({start_burn, PlanId}, _From, #state{plan=undefined,
                                                active_op=undefined,
                                                serial_pid=SerialPid,
                                                corner_alignment=undefined}=State) ->
    case blue_scribe_plan:get_plan(PlanId) of
        {ok, Plan} ->
            {ActiveOp, RestPlan} =
            do_advance_plan(undefined, Plan),
            do_run_op(ActiveOp, SerialPid),
            {reply, ok, State#state{plan=RestPlan,
                                    active_op=ActiveOp}};
        {error, Err} ->
            {reply, {error, Err}, State}
    end;

handle_call({start_burn, _PlanId}, _From, State) ->
    logger:warning("~p: Burn requested while busy. State: ~p", [?MODULE, State]),
    {reply, {error, busy}, State};

handle_call(home, _From, #state{plan=undefined,
                                serial_pid=SerialPid,
                                active_op=undefined}=State) ->
    Res = do_run_op(#laser_command{class='HM', arg1=0, arg2=0}, SerialPid),
    {reply, Res, State};

handle_call(home, _From, #state{plan=Plan}=State) ->
    NewPlan = [#laser_command{class='HM', arg1=0, arg2=0} | Plan],
    logger:info("~p: Inserting HOME into current plan", [?MODULE]),
    {reply, ok, State#state{plan=NewPlan}};

handle_call(corner_alignment, _From, #state{plan=undefined,
                                             active_op=undefined,
                                             corner_alignment=CA}=State) ->
    NewCA = do_corner_alignment(CA),
    {reply, ok, State#state{plan = corner_alignment,
                            corner_alignment=NewCA}};

handle_call(pause_burn, _From, #state{plan=undefined}=State) ->
    {reply, {error, not_running}, State};

handle_call(pause_burn, _From, #state{}=State) ->
    {reply, ok, State#state{paused=true}};

handle_call(resume_burn, _From, #state{plan=undefined}=State) ->
    {reply, {error, not_running}, State};

handle_call(resume_burn, _From, #state{}=State) ->
    %TODO do something to resume?
    {reply, {error, not_implemented}, State#state{paused=false}};

handle_call(Call, _, State) ->
    logger:warning("~p: Unknown call ~p", [?MODULE, Call]),
    {reply, ok, State}.

handle_cast(Cast, State) ->
    logger:warning("~p: Unknown call ~p", [?MODULE, Cast]),
    {noreply, State}.

handle_info({serial_rx_data, BinaryMessage},
            #state{}=State) ->
    {ok, NewState} = do_handle_serial_message(BinaryMessage, State),
    {noreply, NewState}.

-spec do_setup_serial(Path::string(),
                      Baud::serial_baud(),
                      ListenerPid::pid()) -> {ok, pid()}|{error,_}.
do_setup_serial(Path, Baud, _ListenerPid) ->
  io:format("Setting up serial for ~s, @ ~p baud~n",[Path, Baud]),
  {ok, SerialPid} =
  serial:start_link([{open, Path},{speed, Baud},{message_delimiter, "\r\n"}]),
  %serial:send(SerialPid, "AT\r\n"),
  {ok, SerialPid}.

-spec do_handle_serial_message(Message :: binary(), State :: #state{}) ->
    {ok, #state{}} | {error, _}.
do_handle_serial_message(<<"Error\n\r">>, #state{active_op=Op}=State) ->
    logger:error("~p: Microcontroller responded with error. "
                 "Active op: ~p", [?MODULE, Op]),
    State;
do_handle_serial_message(<<"A\n\r">>, #state{plan=Plan,
                                             active_op=Op,
                                             serial_pid=SerialPid,
                                             paused=Paused}=State) ->
    logger:debug("~p: Command completed successfully: ~p",
                 [?MODULE, do_get_next_cmd(Op)]),
    % don't start a new command if we are paused
    % decide on the next command and start it.
    % active_op has commands subtracted as they are completed
    % active op has GO commands added as necessary before being stored in active_op
    {NewActiveOp, NewPlan} =
    do_advance_plan(Op, Plan),
    case {Paused, do_get_next_cmd(NewActiveOp)} of
        {true,_} -> do_nothing;
        {_, undefined} ->
            logger:debug("~p: undefined next cmd", [?MODULE]),
            do_nothing;
        {_, Cmd} ->
            ok = do_run_cmd(Cmd, SerialPid)
    end,
    %TODO support pause/unpause?
    logger:debug("~p: New plan has ~p elements, active op = ~p",
                 [?MODULE, case is_list(NewPlan) of
                               true -> length(NewPlan);
                               false -> undefined
                           end, NewActiveOp]),
    {ok, State#state{plan=NewPlan, active_op=NewActiveOp}}.

-spec do_get_next_cmd(Op :: laser_cmdop()) -> #laser_command{} | undefined.
do_get_next_cmd(#laser_command{}=Cmd) -> Cmd;
do_get_next_cmd(#laser_operation{commands=[NextCmd | _Rest]}) ->
    NextCmd;
do_get_next_cmd(_) ->
    undefined.

%% -----------------------------------------------------------------------------
%% @doc  Some logic about how operations/commands advance in the queue
%% -----------------------------------------------------------------------------
-spec do_advance_plan(ActiveOp :: laser_cmdop() | undefined,
                      Plan :: laser_plan()) ->
    {NewActiveOp :: laser_cmdop()|undefined, NewPlan :: laser_plan()|undefined}.
do_advance_plan(#laser_command{}, []) ->
    {undefined, undefined};
do_advance_plan(#laser_operation{commands=CmdList}, []) when is_list(CmdList) andalso
                                                             length(CmdList) =< 1 ->
    {undefined, undefined};
do_advance_plan(undefined, []) ->
    {undefined, undefined};
do_advance_plan(#laser_command{}, [NextOp | ReducedPlan]) ->
    % just advance, laser command is singular
    logger:debug("~p: Advancing past singular command", [?MODULE]),
    %case ReducedPlan of
    %    [] ->
    %        {undefined, undefined};
    %    _ ->
    %        {do_load_new_op(NextOp), ReducedPlan}
    %end;
    {do_load_new_op(NextOp), ReducedPlan};
do_advance_plan(undefined, [NextOp | ReducedPlan]) ->
    logger:debug("~p: Advancing past undefined", [?MODULE]),
    {do_load_new_op(NextOp), ReducedPlan};
do_advance_plan(#laser_operation{commands=[_LastCmd]}, [NextOp | ReducedPlan]) ->
    logger:debug("~p: Advancing past completed operation", [?MODULE]),
    % operation is completed, advance to next op
    %case ReducedPlan of
    %    [] ->
    %        {undefined, undefined};
    %    _ ->
    %        {do_load_new_op(NextOp), ReducedPlan}
    %end;
    {do_load_new_op(NextOp), ReducedPlan};
do_advance_plan(#laser_operation{commands=[_PastCmd | RestCmd]}=Op, Plan) ->
    logger:debug("~p: Advancing to next CMD in operation", [?MODULE]),
    % more cmds in the op
    % the plan stays the same and the op has its completed cmd removed
    {Op#laser_operation{commands=RestCmd}, Plan}.

%% -----------------------------------------------------------------------------
%% @doc Insert GO command into new ops as needed
%% -----------------------------------------------------------------------------
-spec do_load_new_op(Op :: #laser_operation{} | #laser_command{}) ->
    #laser_operation{} | #laser_command{}.
do_load_new_op(#laser_command{}=Cmd) ->
    Cmd;
do_load_new_op(#laser_operation{start_x=X, start_y=Y, commands=Cmds}=Op) ->
    Op#laser_operation{commands=[#laser_command{class='GO',
                                                arg1=X,
                                                arg2=Y} | Cmds]}.

%% -----------------------------------------------------------------------------
%% @doc Run the next command or next command in an operation by sending it
%% to the serial/uart process
%% -----------------------------------------------------------------------------
-spec do_run_op(OpCmd :: laser_cmdop(), SerialPid :: pid()) ->
    ok | {error,_}.
do_run_op(#laser_command{}=Cmd, Pid) ->
    do_run_cmd(Cmd, Pid);
do_run_op(#laser_operation{commands=[]}, _Pid) ->
    logger:error("~p: do_run_op/1 Empty Op", [?MODULE]),
    {error, empty};
do_run_op(#laser_operation{commands=[NextCmd | _]}, Pid) ->
    do_run_cmd(NextCmd, Pid).

%% -----------------------------------------------------------------------------
%% @doc Send a laser command to the serial/uart process
%% -----------------------------------------------------------------------------
-spec do_run_cmd(Cmd :: #laser_command{}, SerialPid :: pid()) -> ok | {error,_}.
do_run_cmd(Cmd, Pid) ->
    Bin = do_format_command(Cmd),
    serial:send(Pid, Bin).

-spec do_format_command(Cmd :: #laser_command{}) -> binary().
do_format_command(#laser_command{class='HM'}) ->
    <<"HM\n\r">>;
do_format_command(#laser_command{class=Class,
                                 arg1 = Arg1,
                                 arg2 = Arg2}) ->
    Str = io_lib:format("~s ~p ~p\n\r",
                        [atom_to_list(Class), Arg1, Arg2]),
    list_to_binary(Str).

-spec do_corner_alignment(CurrentCorner :: 0..3 | undefined) -> 0..3 | undefined.
do_corner_alignment(undefined) ->
    %do_home
    0;
do_corner_alignment(0) ->
    % do move right
    1;
do_corner_alignment(1) ->
    % do move down
    2;
do_corner_alignment(2) ->
    % do move left
    3;
do_corner_alignment(3) ->
    % move up
    undefined.


-ifdef(TEST).

plan_advancement_test() ->
    ExampleCommand1 = #laser_command{class='BV',
                                     arg1=49,
                                     arg2=0},
    ExampleCommand2 = #laser_command{class='BH',
                                     arg1=20,
                                     arg2=0},
    ExampleOp1 = #laser_operation{start_x=3,
                                  start_y=5,
                                  commands=[ExampleCommand1, ExampleCommand2]},
    ExampleOp1WithGo =
    ExampleOp1#laser_operation{commands=[#laser_command{class='GO',arg1=3,arg2=5},
                                         ExampleCommand1, ExampleCommand2]},
    ExampleOp2 = #laser_operation{start_x=10,
                                  start_y=20,
                                  commands=[ExampleCommand2]},
    TestPlan1 = [ExampleOp1, ExampleOp2],
    ResultPlan1 = [ExampleOp2],%[ExampleOp1#laser_operation{commands=[ExampleCommand2]}],
    ?assertEqual({ExampleOp1WithGo, ResultPlan1}, do_advance_plan(ExampleCommand1, TestPlan1)),

    TestOp2 = ExampleOp1WithGo,
    TestPlan2 = TestPlan1,
    ?assertEqual({ExampleOp1, TestPlan2}, do_advance_plan(TestOp2, TestPlan2)),

    ?assertEqual({undefined, undefined}, do_advance_plan(ExampleCommand1, [])),
    ?assertEqual({ExampleOp1WithGo, []}, do_advance_plan(ExampleCommand1, [ExampleOp1])),
    ExampleOp1Reduced =
    ExampleOp1#laser_operation{commands=[ExampleCommand2]},
    ?assertEqual({ExampleOp1Reduced, TestPlan1}, do_advance_plan(ExampleOp1, TestPlan1)),
    ?assertEqual({ExampleOp1WithGo, [ExampleOp2]}, do_advance_plan(undefined, TestPlan1)).

-endif.%TEST

