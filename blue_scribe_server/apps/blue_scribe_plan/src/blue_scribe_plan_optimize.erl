-module(blue_scribe_plan_optimize).

-include_lib("blue_scribe_burner/include/blue_scribe_burner.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.%TEST

-export([plan_cost/1, greedy_optimize/1]).


-spec plan_cost(Plan :: laser_plan()) -> non_neg_integer().
plan_cost(Plan) ->
    {_, _, Cost} =
    lists:foldl(fun(Op, {PrevX, PrevY, AccCost}) ->
                        UpdatedCost =
                        AccCost + operation_cost(PrevX, PrevY, Op),
                        {NewX, NewY} = destination(PrevX, PrevY, Op),
                        {NewX, NewY, UpdatedCost}
                end,
                {0, 0, 0},
                Plan),
    Cost.

-spec greedy_optimize(Plan :: laser_plan()) -> laser_plan().
greedy_optimize(Plan) ->
    {StartInstruction, RestPlan} = extract_closest_op(0, 0, Plan),
    greedy_optimize_(RestPlan, [StartInstruction]).

-spec greedy_optimize_(UnoptimizedPlan :: laser_plan(),
                       OptimizedPlan :: laser_plan()) ->

    laser_plan().
greedy_optimize_([], OptPlan) ->
    OptPlan;
greedy_optimize_(UnoptPlan, OptPlan) ->
    {LastX, LastY} = destination(0, 0, lists:last(OptPlan)),
    {NextInstruction, RestUnopt} = extract_closest_op(LastX, LastY, UnoptPlan),
    greedy_optimize_(RestUnopt, lists:append(OptPlan, [NextInstruction])).


%% -----------------------------------------------------------------------------
%% @doc Finds nearest command or operation to a certain point.
%% Commands outside of operations have no known starting point,
%% so they are always considered zero distance so they will keep their
%% place at the beginning of the plan if they are present.
%% -----------------------------------------------------------------------------
-spec extract_closest_op(Xs :: non_neg_integer(),
                       Ys :: non_neg_integer(),
                       Plan :: laser_plan()) ->
    {#laser_operation{} | #laser_command{}, laser_plan()}.
extract_closest_op(Xs, Ys, Plan) ->
    DistMap =
    lists:map(fun(#laser_command{} = _Cmd) ->
                      0;
                 (#laser_operation{start_x = Xd,
                                   start_y = Yd} = _Op) ->
                      distance(Xs, Ys, Xd, Yd)
              end,
              Plan),
    MinDist = lists:min(DistMap),
    IndexedDist = lists:zip(DistMap, lists:seq(1,length(DistMap))),
    {MinDist, Idx} = lists:keyfind(MinDist, 1, IndexedDist),
    ClosestOp = lists:nth(Idx, Plan),
    {ClosestOp, lists:delete(ClosestOp, Plan)}.
    

-spec operation_cost(StartX :: non_neg_integer(),
                     StartY :: non_neg_integer(),
                     Operation :: #laser_operation{}) -> non_neg_integer().
operation_cost(StartX, StartY, #laser_operation{start_x=OpX,
                                                start_y=OpY,
                                                commands=Commands}) ->
    RelocateCost = distance(StartX, StartY, OpX, OpY),
    lists:foldl(fun(Cmd, Acc) ->
                        Acc + command_cost(StartX, StartY, Cmd)
                end,
                RelocateCost,
                Commands);
operation_cost(StartX, StartY, LC = #laser_command{}) ->
    command_cost(StartX, StartY, LC).

%% -----------------------------------------------------------------------------
%% @doc Movement cost of a single laser command.
%% Starting X and Y location are considered for HOME and GO only.
%% -----------------------------------------------------------------------------
-spec command_cost(StartX :: non_neg_integer(),
                   StartY :: non_neg_integer(),
                   Command :: #laser_command{}) -> non_neg_integer().
command_cost(Xs, Ys, #laser_command{class = 'GO',
                                  arg1 = Xd,
                                  arg2 = Yd}) -> distance(Xs, Ys, Xd, Yd);
command_cost(Xs, Ys, #laser_command{class = 'HM'}) -> distance(Xs, Ys, 0, 0);
command_cost(_, _, #laser_command{class = 'BH', arg1 = X}) -> X;
command_cost(_, _, #laser_command{class = 'BV', arg1 = Y}) -> Y;
command_cost(_, _, #laser_command{class = 'SQ', arg1 = S}) -> S*4.

%% -----------------------------------------------------------------------------
%% @doc Effective laser hardware distance cost function.
%% Since the laser can move in X and Y at the same time, which ever movement,
%% X or Y, is larger is the one which determines the movement cost.
%% -----------------------------------------------------------------------------
-spec distance(StartX :: non_neg_integer(), StartY :: non_neg_integer(),
               EndX :: non_neg_integer(), EndY :: non_neg_integer()) ->
    non_neg_integer().
distance(StartX, StartY, EndX, EndY) ->
    DX = abs(EndX - StartX),
    DY = abs(EndY - StartY),
    case DX > DY of
        true -> DX;
        false -> DY
    end.

-spec destination(StartX :: non_neg_integer(), StartY :: non_neg_integer(),
                  Command :: #laser_operation{} | #laser_command{}) ->
    {EndX :: non_neg_integer(), EndY :: non_neg_integer()}.
destination(_Xs, _Ys, #laser_command{class = 'GO',
                                   arg1 = Xd,
                                   arg2 = Yd}) -> {Xd, Yd};
destination(_Xs, _Ys, #laser_command{class = 'HM'}) -> {0,0};
destination(Xs, Ys, #laser_command{class = 'BH',
                                   arg1 = Dx}) -> {Xs + Dx, Ys};
destination(Xs, Ys, #laser_command{class = 'BV',
                                   arg1 = Dy}) -> {Xs, Ys + Dy};
destination(Xs, Ys, #laser_command{class = 'SQ'}) -> {Xs, Ys};
destination(_Xs, _Ys, #laser_operation{start_x = Sx,
                                       start_y = Sy,
                                       commands = Cmds}) ->
    lists:foldl(
      fun(Cmd, {X, Y}) ->
              destination(X, Y, Cmd)
      end,
      {Sx, Sy},
      Cmds).


-ifdef(TEST).

distance_test() ->
    ?assertEqual(100, distance(0, 0, 100, 100)),
    ?assertEqual(100, distance(0, 0, 0, 100)),
    ?assertEqual(500, distance(500, 0, 0, 100)).

plan_cost_test() ->
    ExamplePlan1 =
 [{laser_operation,0,0,[{laser_command,'BH',50,510}]},
 {laser_operation,1000,1000,[{laser_command,'BH',700,510}]},
 {laser_operation,1650,1050,[{laser_command,'BH',-50,510}]},
 {laser_operation,1200,1050,[{laser_command,'BH',-50,32}]},
 {laser_operation,1000,1050,[{laser_command,'BH',-50,510}]},
 {laser_operation,1000,1100,[{laser_command,'BH',50,510}]},
 {laser_operation,1650,1150,[{laser_command,'BH',-50,510}]},
 {laser_operation,1200,1150,[{laser_command,'BH',-50,20}]},
 {laser_operation,1000,1150,[{laser_command,'BH',-50,510}]},
 {laser_operation,1000,1200,[{laser_command,'BH',50,510}]},
 {laser_operation,1200,1200,[{laser_command,'BH',50,16}]},
 {laser_operation,1650,1250,[{laser_command,'BH',-50,510}]},
 {laser_operation,1000,1250,[{laser_command,'BH',-50,510}]},
 {laser_operation,1000,1300,[{laser_command,'BH',50,510}]},
 {laser_operation,1650,1350,[{laser_command,'BH',-50,510}]},
 {laser_operation,1000,1350,[{laser_command,'BH',-50,510}]},
 {laser_operation,1000,1400,[{laser_command,'BH',50,510}]},
 {laser_operation,1650,1450,[{laser_command,'BH',-50,510}]},
 {laser_operation,1000,1450,[{laser_command,'BH',-50,510}]},
 {laser_operation,1000,1500,[{laser_command,'BH',700,510}]}],
 ?assertEqual(7650, plan_cost(ExamplePlan1)).

extract_closest_op_test() ->
    ExPlan2 =
    [{laser_operation,1200,1200,[{laser_command,'BH',50,16}]},
     {laser_operation,1650,1250,[{laser_command,'BH',-50,510}]},
     {laser_operation,1000,1100,[{laser_command,'BH',50,510}]},
     {laser_operation,1000,1250,[{laser_command,'BH',-50,510}]}],
    Closest = 
    {laser_operation,1000,1100,[{laser_command,'BH',50,510}]},
    PlanWithoutClosest = lists:delete(Closest, ExPlan2),
    ?assertEqual({Closest, PlanWithoutClosest}, extract_closest_op(0, 0, ExPlan2)),
    ExPlan3 =
    lists:append([ExPlan2, [#laser_command{class='HM', arg1=0, arg2=0}]]),
    Closest3 = #laser_command{class='HM', arg1=0, arg2=0},
    PlanWithoutClosest3 = ExPlan2,
    ?assertEqual({Closest3, PlanWithoutClosest3}, extract_closest_op(2000, 2000, ExPlan3)).

greedy_optimize_test() ->
    ExamplePlan1 =
    [{laser_operation,0,0,[{laser_command,'BH',50,510}]},
     {laser_operation,1000,1000,[{laser_command,'BH',700,510}]},
     {laser_operation,1650,1050,[{laser_command,'BH',-50,510}]},
     {laser_operation,1200,1050,[{laser_command,'BH',-50,32}]},
     {laser_operation,1000,1050,[{laser_command,'BH',-50,510}]},
     {laser_operation,1000,1100,[{laser_command,'BH',50,510}]},
     {laser_operation,1650,1150,[{laser_command,'BH',-50,510}]},
     {laser_operation,1200,1150,[{laser_command,'BH',-50,20}]},
     {laser_operation,1000,1150,[{laser_command,'BH',-50,510}]},
     {laser_operation,1000,1200,[{laser_command,'BH',50,510}]},
     {laser_operation,1200,1200,[{laser_command,'BH',50,16}]},
     {laser_operation,1650,1250,[{laser_command,'BH',-50,510}]},
     {laser_operation,1000,1250,[{laser_command,'BH',-50,510}]},
     {laser_operation,1000,1300,[{laser_command,'BH',50,510}]},
     {laser_operation,1650,1350,[{laser_command,'BH',-50,510}]},
     {laser_operation,1000,1350,[{laser_command,'BH',-50,510}]},
     {laser_operation,1000,1400,[{laser_command,'BH',50,510}]},
     {laser_operation,1650,1450,[{laser_command,'BH',-50,510}]},
     {laser_operation,1000,1450,[{laser_command,'BH',-50,510}]},
     {laser_operation,1000,1500,[{laser_command,'BH',700,510}]}],
    OptimizedPlan1 =
    [{laser_operation,0,0,[{laser_command,'BH',50,510}]},
     {laser_operation,1000,1000,[{laser_command,'BH',700,510}]},
     {laser_operation,1650,1050,[{laser_command,'BH',-50,510}]},
     {laser_operation,1650,1150,[{laser_command,'BH',-50,510}]},
     {laser_operation,1650,1250,[{laser_command,'BH',-50,510}]},
     {laser_operation,1650,1350,[{laser_command,'BH',-50,510}]},
     {laser_operation,1650,1450,[{laser_command,'BH',-50,510}]},
     {laser_operation,1200,1050,[{laser_command,'BH',-50,32}]},
     {laser_operation,1200,1150,[{laser_command,'BH',-50,20}]},
     {laser_operation,1200,1200,[{laser_command,'BH',50,16}]},
     {laser_operation,1000,1050,[{laser_command,'BH',-50,510}]},
     {laser_operation,1000,1100,[{laser_command,'BH',50,510}]},
     {laser_operation,1000,1150,[{laser_command,'BH',-50,510}]},
     {laser_operation,1000,1200,[{laser_command,'BH',50,510}]},
     {laser_operation,1000,1250,[{laser_command,'BH',-50,510}]},
     {laser_operation,1000,1300,[{laser_command,'BH',50,510}]},
     {laser_operation,1000,1350,[{laser_command,'BH',-50,510}]},
     {laser_operation,1000,1400,[{laser_command,'BH',50,510}]},
     {laser_operation,1000,1450,[{laser_command,'BH',-50,510}]},
     {laser_operation,1000,1500,[{laser_command,'BH',700,510}]}],
    ?assertEqual(OptimizedPlan1, greedy_optimize(ExamplePlan1)).

-endif.%TEST
