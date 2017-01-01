%%%-------------------------------------------------------------------
%%% @author rwalski
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. gru 2016 4:25 PM
%%%-------------------------------------------------------------------
-module(ppk).
-author("rwalski").
-compile([export_all]).
%% API
%%-export([]).


create_proxy() ->
  spawn(?MODULE, proxy, []).

createConsumer() ->
  spawn(?MODULE, consumer, []).

send(Pid, Msg) ->
  Pid ! {send, Msg}.

subscribeConsumer(Pid, Cons) ->
  Pid ! {subscribe, Cons}.


proxy() -> proxy([]).
proxy(L) ->
  receive
    {send, Msg} ->
      case length(L) > 0 of
          true -> send_from_proxy(L, Msg);
          false -> io:format("No subscribes found")
      end,
      proxy(L);

    {subscribe, Pid} -> proxy([Pid | L]);

    terminate -> bye_bye
  end.


send_from_proxy([], _) -> ok;
send_from_proxy(L, Msg) ->
  hd(L) ! {msg, Msg}, send_from_proxy(tl(L), Msg).


consumer() ->
  receive
    {msg, Msg} -> io:format("MESSAGE: ~s~n", [Msg]), consumer()
  end.


