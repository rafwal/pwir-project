%%%-------------------------------------------------------------------
%%% @author rwalski
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. gru 2016 11:53 AM
%%%-------------------------------------------------------------------
-module(kitchen).
-author("rwalski").
-compile([export_all]).
%% API
%%-export([]).

fridge1() ->
  receive
    {From, {store, _Food}} -> From ! {self(), ok}, fridge1();
    {From, {take, _Food}} -> From ! {self(), not_found}, fridge1();
    terminate -> ok
  end.

fridge2(List) ->
  receive
    {From, {store, _Food}} ->
      From ! {self(), ok},
      fridge2([_Food | List]);
    {From, {take, _Food}} ->
      case lists:member(_Food, List) of
        true -> From ! {self(), found}, fridge2(lists:delete(_Food, List));
        false -> From ! {self(), not_found}, fridge2(List)
      end;
    terminate -> ok
  end.

store(Pid, _Food) ->
  Pid ! {self(), {store, _Food}},
  receive
    {Pid, Message} -> Message
  after
    3000 -> timeout
  end.

take(Pid, _Food) ->
  Pid ! {self(), {take, _Food}},
  receive
    {Pid, Message} -> Message
  after
    3000 -> timeout
  end.

start(List) -> spawn(?MODULE, fridge2, [List]).

important() ->
  receive
    {Priority, Message} when Priority > 10 ->
      [Message | important()]
  after 0 ->
    normal()
  end.

normal() ->
  receive
    {_, Message} ->
      [Message | normal()]
  after 0 ->
    []
  end.

sword(1) -> throw(slice);
sword(2) -> erlang:error(cut_arm);
sword(3) -> exit(cut_leg);
sword(4) -> throw(punch);
sword(5) -> exit(cross_bridge).

talk() -> "blah blah".

black_knight(Attack) when is_function(Attack, 0) ->
  try Attack() of
    _ -> "None shall pass."
  catch
    throw:slice -> "It is but a scratch.";
    error:cut_arm -> "I've had worse.";
    exit:cut_leg -> "Come on you pansy!";
    _:_ -> "Just a flesh wound."
  end.

simulate() ->
  [black_knight(fun() -> sword(A) end) || A <- lists:seq(1,5)].

simulate2() ->
  X = fun sword/1,
  [black_knight(X(P)) || P <- lists:seq(1,5)].

seq(S,E) ->
  if
    S == E -> [E];
    true -> [S | seq(S + 1, E)]
  end.

seq2(S,E) ->
  seq2(S,E,[]).

seq2(S,E,T) ->
  if
    S == E -> lists:reverse([S | T]);
    true -> seq2(S+1,E,[S | T])
  end.