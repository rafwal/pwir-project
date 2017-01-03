-module(gsAddressBook).
-behaviour(gen_server).

-export([start_link/0, start_link/1, init/1, handle_cast/2, handle_call/3, terminate/2, stop/0]).
-export([addContact/2, addEmail/3, addPhone/3, removeContact/2, removeEmail/1, removePhone/1, addRelation/5, removeRelation/5, getEmails/2, getPhones/2, findByEmail/1, findByPhone/1, getRelations/2, getAddressBook/0, crash/0]).

start() ->
	start(addressBook:createAddressBook()).

start(InitVal) ->
	gen_server:start({local, ab_server}, ?MODULE, InitVal, []).

start_link() ->
	start_link(addressBook:createAddressBook()).

start_link(InitVal) ->
	gen_server:start_link({local, ab_server}, ?MODULE, InitVal, []).

init(AB) ->
	{ok, AB}.

terminate(Reason, AB) ->
	io:format("exit with AB = ~p~n", [AB]),
	Reason.

handle_cast({addContact, [FName, LName]}, AB) ->
	{noreply, handle_err(AB, addressBook:addContact(FName, LName, AB))};

handle_cast({addEmail, [FName, LName, Email]}, AB) ->
	{noreply, handle_err(AB, addressBook:addEmail(FName, LName, Email, AB))};

handle_cast({addPhone, [FName, LName, Phone]}, AB) ->
	{noreply, handle_err(AB, addressBook:addPhone(FName, LName, Phone, AB))};

handle_cast({removeContact, [FName, LName]}, AB) ->
	{noreply, handle_err(AB, addressBook:removeContact(FName, LName, AB))};

handle_cast({removeEmail, [Email]}, AB) ->
	{noreply, handle_err(AB, addressBook:removeEmail(Email, AB))};

handle_cast({removePhone, [Phone]}, AB) ->
	{noreply, handle_err(AB, addressBook:removePhone(Phone, AB))};

handle_cast({addRelation, [FName, LName, Rel, RFName, RLName]}, AB) ->
	{noreply, handle_err(AB, addressBook:addRelation(FName, LName, Rel, RFName, RLName, AB))};

handle_cast({removeRelation, [FName, LName, Rel, RFName, RLName]}, AB) ->
	{noreply, handle_err(AB, addressBook:removeRelation(FName, LName, Rel, RFName, RLName, AB))};

handle_cast(stop, AB) ->
	{stop, normal, AB};

handle_cast({crash, []}, AB) ->
	1/0,
	{noreply, AB}.

handle_call({getEmails, [FName, LName]}, _From, AB) ->
	{reply, addressBook:getEmails(FName, LName, AB), AB};

handle_call({getPhones, [FName, LName]}, _From, AB) ->
	{reply, addressBook:getPhones(FName, LName, AB), AB};

handle_call({findByEmail, [Email]}, _From, AB) ->
	{reply, addressBook:findByEmail(Email, AB), AB};

handle_call({findByPhone, [Phone]}, _From, AB) ->
	{reply, addressBook:findByPhone(Phone, AB), AB};

handle_call({getRelations, [FName, LName]}, _From, AB) ->
	{reply, addressBook:getRelations(FName, LName, AB), AB};

handle_call({getAddressBook}, _From,  AB) ->
	{reply, AB, handle_err(AB, AB)}.

handle_err(AB, {error, Msg}) -> 
	io:format("~p~n", [Msg]),
	AB;
handle_err(_, NewAB) -> NewAB.

%% client API

addContact(FName, LName) ->
	gen_server:cast(ab_server, {addContact, [FName, LName]}).

addEmail(FName, LName, Email) ->
	gen_server:cast(ab_server, {addEmail, [FName, LName, Email]}).

addPhone(FName, LName, Phone) ->
	gen_server:cast(ab_server, {addPhone, [FName, LName, Phone]}).

removeContact(FName, LName) ->
	gen_server:cast(ab_server, {removeContact, [FName, LName]}).

removeEmail(Email) ->
	gen_server:cast(ab_server, {removeEmail, [Email]}).

removePhone(Phone) ->
	gen_server:cast(ab_server, {removePhone, [Phone]}).

getEmails(FName, LName) ->
	gen_server:call(ab_server, {getEmails, [FName, LName]}).

getPhones(FName, LName) ->
	gen_server:call(ab_server, {getPhones, [FName, LName]}).

findByEmail(Email) ->
	gen_server:call(ab_server, {findByEmail, [Email]}).

findByPhone(Phone) ->
	gen_server:call(ab_server, {findByPhone, [Phone]}).

getAddressBook() ->
	gen_server:call(ab_server, {getAddressBook}).

addRelation(FName, LName, Rel, RFName, RLName) ->
	gen_server:cast(ab_server, {addRelation, [FName, LName, Rel, RFName, RLName]}).

removeRelation(FName, LName, Rel, RFName, RLName) ->
	gen_server:cast(ab_server, {removeRelation, [FName, LName, Rel, RFName, RLName]}).

getRelations(FName, LName) ->
	gen_server:call(ab_server, {getRelations, [FName, LName]}).

stop() ->
	gen_server:cast(ab_server, stop).

crash() ->
	gen_server:cast(ab_server, {crash, []}).
