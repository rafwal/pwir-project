-module(server).
-author("rwalski, mpiotrowski").

%-export([]).
-compile([export_all]).
-import(semaphore, [start/0, stop/0, signal/0, wait/0]).


%%starts server and semaphore
startServer(Port) ->
  semaphore:start(),
  Pid = spawn_link(fun() ->
    doStartServer(Port)
  end),
  {ok, Pid}.

%%cretes listening socket on given port and spawns first accepting function
doStartServer(Port) ->
  {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}]),
  spawn(fun() -> acceptor(Listen) end),
  timer:sleep(infinity).

%%accepting connection, checks if any librarian is free, and handles it if yes
acceptor(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  semaphore:wait(),
  spawn(?MODULE, acceptor, [Listen]),
  handle(Socket).


handle(Socket) ->
  Id = getIdFromUser(Socket),
  printOptions(Socket),
  handle(Socket, Id).


getIdFromUser(Socket) ->
  gen_tcp:send(Socket, "Give me your id\n"),
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
      gen_tcp:close(Socket),
      semaphore:signal(),
      quit;
    {tcp, Socket, WrappedId} ->
      Id = checkAndExtractId(WrappedId),
      gen_tcp:send(Socket, "Thank you, id provided\n\n"),
      Id;
    _ ->
      semaphore:signal(),
      quit
  end.


printOptions(Socket) ->
  gen_tcp:send(Socket, "To quit type 'quit'\n"),
  gen_tcp:send(Socket, "To borrow the book type 'borrow: \"<BookName>\" \"<Author>\"\n"),
  gen_tcp:send(Socket, "To return the book type: return: \"<BookName>\" \"<Author>\"\n\n").


handle(Socket, Id) ->
  io:format("ID ~s~n", [Id]),
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
      gen_tcp:close(Socket),
      semaphore:signal();
    {tcp, Socket, Msg} ->
      gen_tcp:send(Socket, Msg),
      handle(Socket, Id);
    _ ->
      semaphore:signal()
  end.

%todo validation
checkAndExtractId(Id) ->
  %I = string:str(Id, "Id: "), %% must be 1 'Id: ------
  RealId = string:substr(binary_to_list(Id), 5),
  RealId.