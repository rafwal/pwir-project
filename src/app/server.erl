%%%-------------------------------------------------------------------
%%% @author rwalski
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. gru 2016 1:09 PM
%%%-------------------------------------------------------------------
-module(server).
-author("rwalski").


%% API
%-export([]).
-compile([export_all]).
-import(semaphore, [start/0, stop/0, signal/0, wait/0]).



startServer(Port) ->
  semaphore:start(),
  Pid = spawn_link(fun() ->
    doStartServer(Port)
  end),
  {ok, Pid}.

doStartServer(Port) ->
  {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}]),
  spawn(fun() -> acceptor(Listen) end),
  timer:sleep(infinity).


acceptor(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),

  semaphore:wait(),
  spawn(?MODULE, acceptor, [Listen]),
  sendGreetings(Socket),
  handle(Socket).

sendGreetings(Socket) ->
  gen_tcp:send(Socket, "Hello~n").


handle(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
      gen_tcp:close(Socket),
      semaphore:signal();
    {tcp, Socket, Msg} ->
      gen_tcp:send(Socket, Msg),
      handle(Socket);
    _ ->
      semaphore:signal()
  end.
