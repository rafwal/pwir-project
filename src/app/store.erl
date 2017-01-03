-module(store).
-author("rwalski, mpiotrowski").

-compile(export_all).

-record(book,{id, author, title, year, userId=none}).
-record(person,{id, name}).

%%interface

listAllBooks() ->
  db ! {self(),listAllBooks},
  receive
    Books -> Books
  end.

listUserBooks(UserId) ->
  db ! {self(), UserId, listUserBooks},
  receive
    {ok, no_books} -> "User does not have ano books borrowed";
    {ok, Books} -> Books
  end.

borrowBook(UserId, BookId) ->
  db ! {self(), UserId, BookId, borrow},
  receive
    {ok, M} -> M;
    {error, M} -> M
  end.

returnBook(UserId, BookId) ->
  db ! {self(), UserId, BookId, return},
  receive
    {ok, M} -> M;
    {error, M} -> M
  end.


%%end of interface

startDB() ->
  register(db, spawn(?MODULE, init, [])).

init() ->
  loop(initDB()).

initDB() ->
  {
    [
      #book{id = "1", title = "Abrakadabra", author = "Jerzy Markowski", year = 2012, userId = "AXZ"},
      #book{id = "2", title = "Juz weekend", author = "Jerzy Markowski", year = 2010},
      #book{id = "3", title = "Harry Potter", author = "Rafal Kaban", year = 2010},
      #book{id = "4", title = "Harry Potter", author = "Rafal Kaban", year = 2010, userId = "AXZ"},
      #book{id = "5", title = "Mega wiksa", author = "Jan Janowski", year = 2010}
    ],
    [
      #person{id = "AXZ", name = "Rafal Walski"},
      #person{id = "QWE", name = "Michal Piotrowski"}
    ]
  }.

loop(DB) ->
  receive
    {Pid, UserId, BookId, borrow} ->  %% BORROWING BOOK
      try
        NewDB = bookBorrowed(DB,BookId,UserId),
        Pid ! {ok, "Successfull"},
        loop(NewDB)
      catch
        throw:not_available -> Pid ! {error, "Book is borrowed"}, loop(DB);
        throw:not_found -> Pid ! {error, "Book not found"}, loop(DB)
      end;

    {Pid, UserId, BookId, return} ->  %%RETURNING BOOK
      try
        NewDB = bookReturned(DB,BookId,UserId),
        Pid ! {ok, "Successfull"},
        loop(NewDB)
      catch
        throw:not_borrowed -> Pid ! {error, "Book is not borrowed"}, loop(DB);
        throw:not_found -> Pid ! {error, "Book not found"}, loop(DB);
        throw:borrowed_by_other -> Pid ! {error, "Book borrowed by other user"}
      end;
    {Pid, UserId, listUserBooks} ->   %%LISTS USER'S BOOKS
      UserBooks = listUserBooks(DB, UserId),
      if
        length(UserBooks) == 0 -> Pid ! {ok, no_books};
        true ->  Pid ! {ok, UserBooks}
      end;
    {Pid, listAllBooks} ->    %%LISTS ALL BOOKS
      Pid ! {ok, listAllBooks(DB)}
  end,
  loop(DB).

listAllBooks(DB) ->
  {Books,_} = DB,
  Books.

listUserBooks(DB, UserId) ->
  {Books,_} = DB,
  lists:filter(
    fun(#book{userId = BookerId}) when UserId =:= BookerId -> true;
       (_) -> false end, Books
  ).




%%Returns new DB, with updated data
bookBorrowed(DB, BookId, UserId) ->
  {Books, Users} = DB,
  {bookBorrowed2(Books, BookId, UserId), Users}.

bookBorrowed2([],_,_) -> throw(not_found);
bookBorrowed2([H | T], BookId, UserId) ->
  if
    H#book.id =:= BookId ->
      if
        H#book.userId == none -> [H#book{userId = UserId}| T];
        true -> throw(not_available)
      end;
    true -> [H | bookBorrowed2(T, BookId, UserId)]
  end.


%%
bookReturned(DB, BookId, UserId) ->
  {Books, Users} = DB,
  {bookReturned2(Books, BookId, UserId), Users}.

bookReturned2([],_,_) -> throw(not_found);
bookReturned2([H | T], BookId, UserId) ->
  if
    H#book.id =:= BookId ->
      if
        H#book.userId == none -> throw(not_borrowed);
        H#book.userId /= UserId -> throw(borrowed_by_other);
        true -> [H#book{userId = none}| T]
      end;
    true -> [H | bookReturned2(T, BookId, UserId)]
  end.
