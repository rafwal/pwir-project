-module(semaphore).
-author("rwalski, mpiotrowski").
-compile(export_all).



start() ->
  register(semaphore, spawn(?MODULE, init, [])),
  register(semaphore_size, spawn(?MODULE, semaphore_size, [4])).

stop() ->
  semaphore ! stop.

wait() ->
  semaphore ! {wait, self()},
  receive ok -> ok end.

signal() ->
  semaphore ! {signal, self()}.

get() ->
  semaphore ! {get, self()},
  receive
    N -> N
  end.

init() ->
  free(0).

free(N) ->
  receive
    {wait, Pid} ->  % client is waiting
      Pid ! ok,
      case N < getSize()-1 of
        true -> free(N+1);
        false -> busy(Pid)
      end;
    {signal, Pid} ->  % notify that client is walking away
      free(N-1);
    {get, Pid} ->    % how many clients are ordering
      Pid ! N,
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
  semaphore_size ! {get, self()},
  receive N ->  N end.

setSize(N) ->
  semaphor_size ! {change, N, self()},
  receive
    ok -> ok;
    error -> error
  end.

semaphore_size(N) ->
  receive
    {get, Pid} ->
      Pid ! N, semaphore_size(N);
    {change, NN, Pid} ->
      if
        NN >= N -> Pid ! ok, semaphore_size(NN);
        NN <  N ->
          semaphore ! {get, self()},
          receive
            SN ->
              if
                SN =< NN -> Pid ! ok, semaphore_size(NN);
                SN > N -> Pid ! error, semaphore_size(NN)
              end
          end
      end
  end.