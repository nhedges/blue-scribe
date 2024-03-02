-module(laser_serial_mockup).

-export([start/1, send/2]).

-spec start(ListenerPid::pid()) -> {ok, pid()}|{error,_}.
start(ListenerPid) ->
  RecvPid = spawn(fun() -> recv_loop(ListenerPid) end),
  {ok, RecvPid}.

-spec send(Message::binary(), Pid::pid()) -> ok | {error,_}.
send(Message, Pid) ->
  Pid ! {send, Message},
  ok.

recv_loop(Pid) ->
  receive
    {'$gen_cast', {send, Message}} when is_list(Message) orelse is_binary(Message) ->
      io:format("UART SEND ~s~n", [Message]),
      timer:sleep(rand:uniform(100)),
      Pid ! {serial_rx_data, <<"A\n\r">>};
    Message ->
      io:format("~p: Unknown command:~p~n", [?MODULE, Message])
  end,
  recv_loop(Pid).

