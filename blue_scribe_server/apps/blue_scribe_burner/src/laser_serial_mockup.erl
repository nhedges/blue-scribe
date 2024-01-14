-module(laser_serial_mockup).

-export([start/1, send/2]).

-spec start(ListenerPid::pid()) -> {ok, pid()}|{error,_}.
start(ListenerPid) ->
  RecvPid = spawn(fun recv_loop/0),
  {ok, RecvPid}.

-spec send(Message::binary(), Pid::pid()) -> ok | {error,_}.
send(Message, Pid) ->
  Pid ! {send, Message, self()},
  ok.

recv_loop() ->
  receive
    {send, Message, Pid} when is_list(Message) ->
      io:format("UART SEND ~s~n", [Message]),
      timer:sleep(rand:uniform(3000)),
      Pid ! {serial_rx_data, <<"A\r\n">>};
    Message ->
      io:format("Unknown command:~p~n", [Message])
  end,
  recv_loop().

