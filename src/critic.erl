%%%-------------------------------------------------------------------
%%% @author rwalski
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. gru 2016 2:05 PM
%%%-------------------------------------------------------------------
-module(critic).
-author("rwalski").

%% API
-compile([export_all]).

start_critic() ->
  spawn(?MODULE, restarter, []).


judge(Band, Album) ->
  Pid = whereis(critic),
  Pid !  {self(), {Band, Album}},
  receive
    {Pid, Criticism} -> Criticism
  after 2000 ->
    timeout
  end.

critic() ->
  receive
    {From, {"Rage Against the Turing Machine", "Unit Testify"}} ->
      From ! {self(), "They are great!"};
    {From, {"System of a Downtime", "Memoize"}} ->
      From ! {self(), "They're not Johnny Crash but they're good."};
    {From, {"Johnny Crash", "The Token Ring of Fire"}} ->
      From ! {self(), "Simply incredible."};
    {From, {_Band, _Album}} ->
      From ! {self(), "They are terrible!"}
  end,
  critic().

restarter() ->
  process_flag(trap_exit, true),
  Pid = spawn_link(?MODULE, critic, []),
  register(critic, Pid),
  receive
    {'EXIT', Pid, normal} -> % not a crash
      ok;
    {'EXIT', Pid, shutdown} -> % manual termination, not a crash
      ok;
    {'EXIT', Pid, _} ->
      restarter()
  end.
