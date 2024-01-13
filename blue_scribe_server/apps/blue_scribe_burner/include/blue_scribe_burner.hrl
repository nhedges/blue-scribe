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

-type laser_cmdop() :: #laser_operation{} | #laser_command{}.
-type laser_plan() :: [#laser_operation{} | #laser_command{}].

-type serial_baud() :: 300|1200|4800|9600|19200|28800|38400|57600|115200.

-endif.%?BLUE_SCRIBE_BURNER_HRL
