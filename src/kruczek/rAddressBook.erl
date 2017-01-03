-module(rAddressBook).
%%-import(addressBook, [createAddressBook/0, addContact/3, containsName/2, addEmail/4, addPhone/4, removeContact/3, findByEmail/2, findByPhone/2, removeEmail/2, removePhone/2, getEmails/3, getPhones/3, addRelation/6, removeRelation/6, findTupleByName/2]).

-export([start/0, stop/0, init/0]).
-export([addContact/2, addEmail/3, addPhone/3, getEmails/2, getPhones/2, removeContact/2, removeEmail/1, removePhone/1, findByEmail/1, findByPhone/1, addRelation/5, removeRelation/5, getRelations/2, crash/0, getAddressBook/0]).

init() ->
	loop(addressBook:createAddressBook()).

start() ->
	register(ab_server, spawn(rAddressBook, init, [])).

loop(AB) ->
	receive
		{sync, Msg, Pid} -> 
			loop(handle_err(AB, handle_call(AB, Msg, Pid)));
		{async, Msg} ->
			loop(handle_err(AB, handle_cast(AB, Msg)));
		stop -> 
			terminate()
	end.

stop() ->
	ab_server ! stop.

terminate() ->
	ok.

handle_err(AB, {error, Msg}) ->
	io:format("~p~n", [Msg]),
	AB;
handle_err(_, NewAB) -> NewAB.

%% handle_cast
handle_cast(AB, {addContact, [FName, LName]}) ->
	addressBook:addContact(FName, LName, AB);

handle_cast(AB, {addEmail, [FName, LName, Mail]}) -> 
	addressBook:addEmail(FName, LName, Mail, AB);

handle_cast(AB, {addPhone, [FName, LName, Phone]}) -> 
	addressBook:addPhone(FName, LName, Phone, AB);

handle_cast(AB, {removeContact, [FName, LName]}) ->
	addressBook:removeContact(FName, LName, AB);

handle_cast(AB, {removeEmail, [Email]}) ->
	addressBook:removeEmail(Email, AB);	

handle_cast(AB, {removePhone, [Phone]}) ->
	addressBook:removePhone(Phone, AB);	

handle_cast(AB, {addRelation, [FName, LName, Rel, RFName, RLName]}) ->
	addressBook:addRelation(FName, LName, Rel, RFName, RLName, AB);

handle_cast(AB, {removeRelation, [FName, LName, Rel, RFName, RLName]}) ->
	addressBook:removeRelation(FName, LName, Rel, RFName, RLName, AB);


handle_cast(_AB, {crash, []}) ->					%%mozna zamienic krotke na sam atom
	1/0;

handle_cast(AB, _) -> AB.

%% handle_call

handle_call(AB, {getEmails, [FName, LName]}, Pid) ->
	reply(AB, addressBook:getEmails(FName, LName, AB), Pid);
	

handle_call(AB, {getPhones, [FName, LName]}, Pid) ->
	reply(AB, addressBook:getPhones(FName, LName, AB), Pid);

handle_call(AB, {getAddressBook, []}, Pid) ->		%%mozna zamienic krotke na sam atom
	reply(AB, AB, Pid);

handle_call(AB, {findByEmail, [Email]}, Pid) ->
	reply(AB, addressBook:findByEmail(Email, AB), Pid);

handle_call(AB, {findByPhone, [Phone]}, Pid) ->
	reply(AB, addressBook:findByPhone(Phone, AB), Pid);

handle_call(AB, {getRelations, [FName, LName]}, Pid) ->
	reply(AB, addressBook:getRelations(FName, LName, AB), Pid);

handle_call(AB, _, Pid) ->
	Pid ! {error, "Bad call!"},
	AB.

reply(AB, Reply, Pid) ->
	Pid ! {reply, Reply},
	AB.


%%

call(Msg) ->
	ab_server ! {sync, Msg, self()},
	receive
		{reply, Reply} -> Reply;
		{error, Error} -> {error, Error}
	end.

cast(Msg) ->
	ab_server ! {async, Msg}.
		
%%

addContact(FName, LName) ->
	cast({addContact, [FName, LName]}).

addEmail(FName, LName, Mail) ->
	cast({addEmail, [FName, LName, Mail]}).

addPhone(FName, LName, Phone) ->
	cast({addPhone, [FName, LName, Phone]}).

getEmails(FName, LName) ->
	call({getEmails, [FName, LName]}).
	
getPhones(FName, LName) ->
	call({getPhones, [FName, LName]}).

removeContact(FName, LName) ->
	cast({removeContact, [FName, LName]}).

removeEmail(Email) ->
	cast({removeEmail, [Email]}).

removePhone(Phone) ->
	cast({removePhone, [Phone]}).

findByEmail(Email) ->
	call({findByEmail, [Email]}).

findByPhone(Phone) ->
	call({findByPhone, [Phone]}).

addRelation(FName, LName, Rel, RFName, RLName) ->
	cast({addRelation, [FName, LName, Rel, RFName, RLName]}).

removeRelation(FName, LName, Rel, RFName, RLName) ->
	cast({removeRelation, [FName, LName, Rel, RFName, RLName]}).

getRelations(FName, LName) ->
	call({getRelations, [FName, LName]}).

getAddressBook() ->
	call({getAddressBook, []}).

crash() ->
	cast({crash, []}).


