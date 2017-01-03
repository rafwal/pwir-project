-module(rAddressBookSup).
-export([start/0, init/0, stop/0]).

start() ->
	register(ab_server_sup, spawn(rAddressBookSup, init, [])).

init() ->
	process_flag(trap_exit, true),
	loop().

loop() ->
	register(ab_server, spawn_link(rAddressBook, init, [])),
	receive
		{'EXIT', _, Reason} -> 
			io:format("Error in ab_server: ~p~n trying to restart", [Reason]),
			loop();
		stop -> ok
	end.

stop() ->
	ab_server_sup ! stop.

%% zadanie
% addressBookSup
% testyAddressBook
% rAddressBook
% rAddressBookSup
% gen_server AB
% supervisor OTP