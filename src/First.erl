%%%-------------------------------------------------------------------
%%% @author rwalski
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. gru 2016 11:44 AM
%%%-------------------------------------------------------------------

-module('First').
-author("rwalski").
-compile([export_all]).
%% API
%-export([helloWorld/0, second/1, print/1, print2/1, fun1/1, bubble_sort_full/1, split/3, merge_sort/1]).


helloWorld(Name) -> timer:sleep(100), io:format("hello world ~s~n", [Name]).

second([]) -> {pusta_lista};
second([_]) -> {jeden_element};
second([_,S|_]) -> S.

print([]) -> ok;
print(L) ->
  io:write(hd(L)),
  print(tl(L)).

print2([]) -> [];
print2(L) -> [hd(L)|print2(tl(L))].

fun1(X) when X > 13 -> true;
fun1(_) -> false.

bubble_sort_full(L) ->
  Length = length(L),
  bubble_sort_full(L, Length).

bubble_sort_full(L, Length) ->
  if
    Length == 0 -> L;
    true -> bubble_sort_full(bubble_sort(L), Length-1)
  end.

bubble_sort([]) -> [];
bubble_sort([F]) -> [F];
bubble_sort([F,S|T]) ->
  if
    F > S -> [S | bubble_sort([F|T])];
    true -> [F | bubble_sort([S|T])]
  end.

split(Number, Slice1, Slice2) ->
  if
    Number == 0 -> [lists:reverse(Slice1), Slice2];
    true -> split(Number-1, [hd(Slice2) | Slice1], tl(Slice2))
  end.


merge_sort([]) -> [];
merge_sort([F]) -> [F];
merge_sort(L) ->
  [L1,L2] = split(length(L) div 2,[],L),
  merge(merge_sort(L1),merge_sort(L2)).

merge(L1, L2) -> merge(L1,L2,[]).

merge(L1,L2,R) ->
  if
    (length(L1) == 0) and (length(L2) == 0) -> lists:reverse(R);
    length(L1) == 0 -> merge(L1,tl(L2), [hd(L2) | R]);
    length(L2) == 0 -> merge(tl(L1),L2, [hd(L1) | R]);
    hd(L1) > hd(L2) -> merge(tl(L1), L2, [hd(L1) | R]);
    true  -> merge(L1, tl(L2), [hd(L2) | R])
  end.


sum([]) -> 0;
sum(L) -> hd(L) + sum(tl(L)).

sum1(L) -> sum1(L,0).

sum1([], S) -> S;
sum1(L, S) -> sum1(tl(L), S + hd(L)).

count(L, X) ->
  count(L,X,0).

count(L,X,C) ->
  if
    length(L) == 0 -> C;
    hd(L) == X -> count(tl(L), X, C+1);
    true -> count(tl(L), X, C)
end.

count2(L,X) ->
  if
    length(L) == 0 -> 0;
    hd(L) == X -> 1 + count2(tl(L), X);
    true -> count2(tl(L), X)
  end.

dolphin1() ->
  receive
    {From, do_a_flip} -> From ! "How 'bout no", dolphin1();
    {From, _} -> From ! "We're smarter", dolphin1()
  end.





myproc() ->
  timer:sleep(5000),
  exit(reason).