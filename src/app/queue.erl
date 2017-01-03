-module(queue).
-author("rwalski, mpiotrowski").
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
