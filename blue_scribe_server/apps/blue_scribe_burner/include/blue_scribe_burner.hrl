-ifndef(BLUE_SCRIBE_BURNER_HRL).
-define(BLUE_SCRIBE_BURNER_HRL, true).

-type laser_command_class() :: 'GO' | 'HM' | 'SQ' | 'BH' | 'BV'.

-record(laser_command,
        {class :: laser_command_class(),
         arg1 :: integer(),
         arg2 :: integer()}).

-record(laser_operation,
        {start_x :: integer() | undefined,
         start_y :: integer() | undefined,
         commands :: [#laser_command{}]}).

-type laser_plan() :: [#laser_operation{} | #laser_command{}].

-endif.%?BLUE_SCRIBE_BURNER_HRL
