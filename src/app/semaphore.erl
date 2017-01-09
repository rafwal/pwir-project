-module(semaphore).
-author("rwalski, mpiotrowski").
-compile(export_all).



start() ->
  register(semaphore, spawn(?MODULE, init, [])),
  register(semaphore_size, spawn(?MODULE, semaphore_size, [4])).

stop() ->
  semaphore ! stop.

wait() ->
  Pid = whereis(semaphore),
  Pid ! {wait, self()},
  receive {ok, Pid} -> ok end.

signal() ->
  semaphore ! {signal, self()}.

get() ->Pid = whereis(semaphore),
  Pid ! {get, self()},
  receive
    {N, Pid} -> N
  end.

init() ->
  free(0).

free(N) ->
  receive
    {wait, Pid} ->  % client is waiting
      Pid ! {ok, self()},
      case N < getSize()-1 of
        true -> free(N+1);
        false -> busy(Pid)
      end;
    {signal, Pid} ->  % notify that client is walking away
      free(N-1);
    {get, Pid} ->    % how many clients are ordering
      Pid ! {N, self()},
      free(N);
    stop ->
      terminate()

  end.

busy(Pid) ->

  receive
    {signal, Pid} ->
      free(getSize()-1);
    {get, Pid} ->    % how many clients are ordering
      Pid ! getSize(),
      busy(Pid)
  end.


terminate() ->
  receive
    {wait, Pid} ->
      exit(Pid, kill),
      terminate()
  after 0 ->
    ok
  end.


%% modyfying size of semafor dynamicly


getSize() ->
  Pid = whereis(semaphore_size),
  Pid ! {get, self()},
  receive {N, Pid} ->  N end.

setSize(N) ->
  Pid = whereis(semaphore_size),
  Pid ! {change, N, self()},
  receive
    {ok, Pid} -> ok;
    {error, Pid} -> error
  end.

semaphore_size(N) ->
  receive
    {get, Pid} ->
      Pid ! {N, self()}, semaphore_size(N);
    {change, NN, Pid} ->
      if
        NN >= N -> Pid ! {ok, self()}, semaphore_size(NN);
        NN <  N ->
          Semaphore_Pid = whereis(semaphore),
          Semaphore_Pid ! {get, self()},
          receive
            {SN, Semaphore_Pid} ->
              if
                SN =< NN -> Pid ! {ok, self()}, semaphore_size(NN);
                true -> Pid ! {error, self()}, semaphore_size(N)
              end
          end
      end
  end.