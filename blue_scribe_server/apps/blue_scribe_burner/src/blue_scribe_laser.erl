-module(blue_scribe_laser).
-behaviour(gen_server).

-export([start_link/0, start_burn/2, home/0, corner_align_next/1, pause_burn/0,
         resume_burn/0, cancel_burn/0, stop/0, status/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%TEMP
-export([do_setup_serial/3]).

-define(MOTOR_SCALE, 50).

-include_lib("blue_scribe_burner/include/blue_scribe_burner.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.%TEST

-record(state,{serial_pid :: pid() | undefined,
               planId :: non_neg_integer() | {corner_alignment, non_neg_integer()} | undefined,
               plan :: laser_plan() | undefined,
               active_op :: laser_cmdop() | undefined,
               opsCompleted = 0 :: non_neg_integer(),
               corner_alignment :: {0..3, non_neg_integer(), non_neg_integer()} | undefined,
               powerScale=1.0 :: float(),
               paused = false :: boolean()}).

-spec start_burn(PlanId :: non_neg_integer(), PowerScale :: float()) -> ok | {error,_}.
start_burn(PlanId, PowerScale) ->
    gen_server:call(?MODULE, {start_burn, PlanId, PowerScale}).

-spec home() -> ok | {error,_}.
home() ->
    gen_server:call(?MODULE, home).

-spec corner_align_next(PlanId :: non_neg_integer()) -> ok | {error,_}.
corner_align_next(PlanId) ->
    gen_server:call(?MODULE, {corner, PlanId}).

-spec pause_burn() -> ok | {error, _}.
pause_burn() ->
    gen_server:call(?MODULE, pause_burn).

-spec resume_burn() -> ok | {error, _}.
resume_burn() ->
    gen_server:call(?MODULE, resume_burn).

-spec cancel_burn() -> ok | {error, _}.
cancel_burn() ->
    gen_server:call(?MODULE, cancel_burn).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

-spec status() -> {ok, laser_status()} | {error, _}.
status() ->
    gen_server:call(?MODULE, status).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, SerialPid} = laser_serial_mockup:start(self()),
    {ok, #state{serial_pid=SerialPid}}.

handle_call(_, _From, #state{serial_pid=undefined}=State) ->
    {reply, {error, no_serial}, State};

handle_call({start_burn, PlanId, PowerScale}, _From, #state{plan=undefined,
                                                active_op=undefined,
                                                serial_pid=SerialPid,
                                                corner_alignment=undefined}=State) ->
    case blue_scribe_plan:get_plan(PlanId) of
        {ok, Plan} ->
            blue_scribe_plan_db:increment_plan_start_counter(PlanId),
            {ActiveOp, RestPlan} =
            do_advance_plan(undefined, Plan),
            do_run_op(ActiveOp, PowerScale, SerialPid),
            {reply, ok, State#state{plan=RestPlan,
                                    planId=PlanId,
                                    active_op=ActiveOp,
                                    opsCompleted=0,
                                    powerScale=PowerScale}};
        {error, Err} ->
            {reply, {error, Err}, State}
    end;

handle_call({start_burn, _PlanId, _PowerScale}, _From, State) ->
    logger:warning("~p: Burn requested while busy. State: ~p", [?MODULE, State]),
    {reply, {error, busy}, State};

handle_call(home, _From, #state{plan=undefined,
                                powerScale=PowerScale,
                                serial_pid=SerialPid,
                                active_op=undefined}=State) ->
    Res = do_run_op(#laser_command{class='HM', arg1=0, arg2=0}, PowerScale, SerialPid),
    Op = #laser_command{class='HM', arg1=0, arg2=0},
    NewPlan = [],
    {reply, Res, State#state{plan=NewPlan, active_op=Op}};

handle_call(home, _From, #state{plan=Plan}=State) ->
    NewPlan = [#laser_command{class='HM', arg1=0, arg2=0} | Plan],
    logger:info("~p: Inserting HOME into current plan", [?MODULE]),
    {reply, ok, State#state{plan=NewPlan}};

handle_call({corner, CornerPlanId}, _From, #state{plan=undefined,
                                                  active_op=undefined,
                                                  serial_pid=SerialPid,
                                                  corner_alignment=undefined}=State) ->
    case blue_scribe_plan:get_dimensions(CornerPlanId) of
        {ok, {Width, Height}} ->
            logger:info("~p: Starting corner alignment for plan ~p with dimensions ~p x ~p",
                        [?MODULE, CornerPlanId, Width, Height]),
            {RunOpRes, Op, NewCa} = do_corner_alignment(undefined, Width, Height, SerialPid),
            NewPlan = [],
            {reply, RunOpRes, State#state{planId={corner_alignment, CornerPlanId},
                                    active_op=Op,
                                    plan=NewPlan,
                                    corner_alignment={NewCa, Width, Height}}};
        {error, Err} ->
            {reply, {error, Err}, State}
    end;

handle_call({corner, CornerPlanId}, _From, #state{planId={corner_alignment, CornerPlanId},
                                                  serial_pid=SerialPid,
                                                  active_op=undefined,
                                                  corner_alignment={CA, W, H}}=State) ->
    {RunOpRes, Op, NewCa} = do_corner_alignment(CA, W, H, SerialPid),
    case NewCa of
        undefined ->
            {reply, RunOpRes, State#state{planId=undefined,
                                          plan=[],
                                          active_op=Op,
                                          corner_alignment=undefined}};
        Num when is_integer(Num) ->
            {reply, RunOpRes, State#state{active_op=Op,
                                          plan=[],
                                          corner_alignment={NewCa, W, H}}}
    end;

handle_call({corner, _CornerPlanId}, _From, State) ->
    {reply, {error, busy}, State};

handle_call(pause_burn, _From, #state{plan=undefined}=State) ->
    {reply, {error, not_running}, State};

handle_call(pause_burn, _From, #state{}=State) ->
    {reply, ok, State#state{paused=true}};

handle_call(resume_burn, _From, #state{plan=undefined}=State) ->
    {reply, {error, no_plan}, State};

handle_call(resume_burn, _From, #state{paused=false}=State) ->
    {reply, {error, not_paused}, State};

handle_call(resume_burn, _From, #state{paused=true,
                                       powerScale=PowerScale,
                                       serial_pid=SerialPid,
                                       plan=Plan}=State) ->
    {ActiveOp, RestPlan} =
    do_advance_plan(undefined, Plan),
    do_run_op(ActiveOp, PowerScale, SerialPid),
    {reply, ok, State#state{paused=false,
                            plan=RestPlan,
                            active_op=ActiveOp}};

handle_call(cancel_burn, _From, #state{powerScale=PowerScale,
                                       serial_pid=SerialPid,
                                       plan=_Plan}=State) ->
    logger:info("~p: Cancelling burn", [?MODULE]),
    Op = #laser_command{class='HM', arg1=0, arg2=0},
    Res = do_run_op(Op, PowerScale, SerialPid),
    {reply, Res, State#state{paused=false,
                             planId=undefined,
                             corner_alignment=undefined,
                             active_op=undefined,
                             plan=undefined}};

handle_call(status, _From, #state{planId={corner_alignment, PlanId},
                                  corner_alignment={Corner,_,_}}=State) ->
    Result = [{planId, PlanId},
              {corner, Corner}],
    {reply, {ok, Result}, State};

handle_call(status, _From, #state{planId=PlanId,
                                  plan=Plan,
                                  powerScale=PowerScale,
                                  opsCompleted=CompletedOps,
                                  paused=Paused}=State) ->
    {OpsRemaining, TimeRemaining} =
    if Plan == undefined -> {0, 0};
       true -> {length(Plan),
                round(blue_scribe_plan:get_plan_time_estimate_seconds(Plan))}
    end,
    Result =
    [{planId, PlanId},
     {paused, Paused},
     {powerScale, PowerScale},
     {opsRemaining, OpsRemaining},
     {opsCompleted, CompletedOps},
     {timeRemaining, tuple_to_list(calendar:seconds_to_time(TimeRemaining))}],
    {reply, {ok, Result}, State};

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
  logger:info("~p: Setting up serial for ~s, @ ~p baud~n",[?MODULE, Path, Baud]),
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
                                             planId=PlanId,
                                             active_op=Op,
                                             opsCompleted=CompletedOps,
                                             serial_pid=SerialPid,
                                             powerScale=PowerScale,
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
            ok = do_run_cmd(Cmd, PowerScale, SerialPid)
    end,
    logger:debug("~p: New plan has ~p elements, active op = ~p",
                 [?MODULE, case is_list(NewPlan) of
                               true -> length(NewPlan);
                               false -> undefined
                           end, NewActiveOp]),
    case {Op, NewActiveOp, NewPlan} of % increment if the plan is finishing (not cancelled)
        {AO, undefined, undefined} when AO =/= undefined ->
            logger:debug("~p: Incremeting plan counter for ~p", [?MODULE, PlanId]),
            blue_scribe_plan_db:increment_plan_finish_counter(PlanId);
        _ -> ok
    end,
    {ok, State#state{plan=NewPlan,
                     active_op=NewActiveOp,
                     opsCompleted=CompletedOps+1}}.

-spec do_get_next_cmd(Op :: laser_cmdop()|undefined) -> #laser_command{} | undefined.
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
do_advance_plan(undefined, undefined) ->
    {undefined, undefined};
do_advance_plan(#laser_command{}, [NextOp | ReducedPlan]) ->
    % just advance, laser command is singular
    logger:debug("~p: Advancing past singular command", [?MODULE]),
    {do_load_new_op(NextOp), ReducedPlan};
do_advance_plan(undefined, [NextOp | ReducedPlan]) ->
    logger:debug("~p: Advancing past undefined", [?MODULE]),
    {do_load_new_op(NextOp), ReducedPlan};
do_advance_plan(#laser_operation{commands=[_LastCmd]}, [NextOp | ReducedPlan]) ->
    logger:debug("~p: Advancing past completed operation", [?MODULE]),
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
-spec do_run_op(OpCmd :: laser_cmdop(),
                PowerScale :: float(),
                SerialPid :: pid()) ->
    ok | {error,_}.
do_run_op(#laser_command{}=Cmd, PowerScale, Pid) ->
    do_run_cmd(Cmd, PowerScale, Pid);
do_run_op(#laser_operation{commands=[]}, _PowerScale, _Pid) ->
    logger:error("~p: do_run_op/1 Empty Op", [?MODULE]),
    {error, empty};
do_run_op(#laser_operation{commands=[NextCmd | _]}, PowerScale, Pid) ->
    do_run_cmd(NextCmd, PowerScale, Pid).

%% -----------------------------------------------------------------------------
%% @doc Send a laser command to the serial/uart process
%% -----------------------------------------------------------------------------
-spec do_run_cmd(Cmd :: #laser_command{},
                 PowerScale :: float(),
                 SerialPid :: pid()) -> ok | {error,_}.
do_run_cmd(Cmd, PowerScale, Pid) ->
    Bin = do_format_command(do_scale_power(Cmd, PowerScale)),
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

%% -----------------------------------------------------------------------------
%% @doc Scales the power parameter of a command up according to the power
%% scale multiplier and rounds to integer.
%% The maximum power value supported by hardware is not considered.
%% Commands which don't have a power parameter are returned unchanged.
%% -----------------------------------------------------------------------------
-spec do_scale_power(Cmd :: #laser_command{}, PowerScale :: float()) -> #laser_command{}.
do_scale_power(#laser_command{class='GO'}=Cmd, _) -> Cmd;
do_scale_power(#laser_command{class='HM'}=Cmd, _) -> Cmd;
do_scale_power(#laser_command{arg2=Power}=Cmd, Scale) ->
    NewPower = round(Power * Scale),
    Cmd#laser_command{arg2=NewPower}.



-spec do_corner_alignment(CurrentCorner :: 0..3 | undefined,
                          Width :: non_neg_integer(),
                          Height :: non_neg_integer(),
                          SerialPid :: pid()) ->
    {ok|{error,_}, laser_cmdop(), 0..3 | undefined}.
do_corner_alignment(undefined, _W, _H, SerialPid) ->
    %do_home
    Op = #laser_command{class='HM', arg1=0, arg2=0},
    RunOpRes = do_run_cmd(Op, 1.0, SerialPid),
    {RunOpRes, Op, 0};

do_corner_alignment(0, W, _H, SerialPid) ->
    % do move right
    Op = #laser_command{class='GO',
                        arg1=W*?MOTOR_SCALE,
                        arg2=0},
    RunOpRes = do_run_cmd(Op, 1.0, SerialPid),
    {RunOpRes, Op, 1};

do_corner_alignment(1, W, H, SerialPid) ->
    % do move down
    Op = #laser_command{class='GO',
                        arg1=W*?MOTOR_SCALE,
                        arg2=H*?MOTOR_SCALE},

    RunOpRes = do_run_cmd(Op, 1.0, SerialPid),
    {RunOpRes, Op, 2};

do_corner_alignment(2, _W, H, SerialPid) ->
    % do move left
    Op = #laser_command{class='GO',
                        arg1=0,
                        arg2=H*?MOTOR_SCALE},
    RunOpRes = do_run_cmd(Op, 1.0, SerialPid),
    {RunOpRes, Op, 3};

do_corner_alignment(3, _W, _H, SerialPid) ->
    % move up
    Op = #laser_command{class='GO',
                        arg1=0,
                        arg2=0},
    RunOpRes = do_run_cmd(Op, 1.0, SerialPid),
    {RunOpRes, Op, undefined}.


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

