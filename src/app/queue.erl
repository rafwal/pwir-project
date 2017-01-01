%%%-------------------------------------------------------------------
%%% @author rwalski
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. sty 2017 8:48 PM
%%%-------------------------------------------------------------------
-module(queue).
-author("rwalski").

%% API
-compile(export_all).

start() ->
  register(queue, spawn(?MODULE, init, [])).

init() ->
  queue(queue:new()).

queue(Q) ->
  receive
    {add, E} ->
      queue(queue:in(E,Q));
    {get, Pid} ->
      {{value: Item}, Q2} = queue:get(Q),
      Pid ! Item,
      queue(Q2)
  end.
