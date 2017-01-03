-module(addressBook).
-export([createAddressBook/0, addContact/3, containsName/2, addEmail/4, addPhone/4, removeContact/3, findByEmail/2, findByPhone/2, removeEmail/2, removePhone/2, getEmails/3, getPhones/3, addRelation/6, removeRelation/6, getRelations/3, findTupleByName/2]).

-record(person, {name, mails=[], phones=[], relations=[]}).
-record(relation, {name, relname}).

createAddressBook() -> [].

addContact(FName, LName, AB) -> 
	case containsName(FName, LName, AB) of
		false -> [#person{name = joinName(FName, LName)} | AB];
		true  -> {error, {contact_exists, "Taki kontakt juz istnieje w bazie"}}
	end.

addEmail(FName, LName, Mail, AB) ->
	case containsMail(Mail, AB) of
		false ->
			AB1 = addContact(FName, LName, AB),
			AB2 = case AB1 of
					{error, {contact_exists, _}} -> AB;
					_ -> AB1
				end,
			Person = findTupleByName(FName, LName, AB2),
			lists:keyreplace(joinName(FName, LName), #person.name, AB2, Person#person{mails = [Mail | Person#person.mails]});
		true -> {error, {mail_exists, "Taki mail juz istnieje w bazie"}}
	end.

addPhone(FName, LName, Phone, AB) -> 
	case containsPhone(Phone, AB) of
		false -> 
			AB1 = addContact(FName, LName, AB),
			AB2 = case AB1 of
					{error, {contact_exists, _}} -> AB;
					_ -> AB1
				end,
			Person = findTupleByName(joinName(FName, LName), AB2),
			lists:keyreplace(joinName(FName, LName), #person.name, AB2, Person#person{phones = [Phone | Person#person.phones]});
		true -> {error, {phone_exists, "Taki telefon juz istnieje w bazie"}}
	end.

removeContact(FName, LName, AB) -> removeContact(joinName(FName, LName), AB).
removeContact(Name, AB) ->
	case containsName(Name, AB) of
		true -> lists:keydelete(Name, #person.name, AB);
		false -> {error, {contact_not_exists, "Nie ma takiego kontaktu w bazie"}}
	end.

removeEmail(Mail, AB) ->
	Person = findTupleByEmail(Mail, AB),
	case Person of
		{error, _} -> Person;
		_ -> lists:keyreplace(Person#person.name, #person.name, AB, Person#person{mails = lists:delete(Mail, Person#person.mails)})
	end.

removePhone(Phone, AB) ->
	Person = findTupleByPhone(Phone, AB),
	case Person of
		{error, _} -> Person;
		_ -> lists:keyreplace(Person#person.name, #person.name, AB, Person#person{phones = lists:delete(Phone, Person#person.phones)})
	end.

getEmails(LName, FName, AB) ->
	Person = findTupleByName(LName, FName, AB),
	case Person of 
		false -> {error, {contact_not_exists, "Nie ma takiego kontaktu w bazie"}};
		_ -> Person#person.mails
	end.

getPhones(LName, FName, AB) ->
	Person = findTupleByName(LName, FName, AB),
	case Person of 
		false -> {error, {contact_not_exists, "Nie ma takiego kontaktu w bazie"}};
		_ -> Person#person.phones
	end.
	
findByEmail(Mail, AB) ->
	Person = findTupleByEmail(Mail, AB),
	case Person of
		{error, _} -> Person;
		_ -> Person#person.name
	end.

findByPhone(Phone, AB) ->
	Person = findTupleByPhone(Phone, AB),
	case Person of
		{error, _} -> Person;
		_ -> Person#person.name
	end.

%% modyfikacje

addRelation(FName, LName, Rel, RFName, RLName, AB) ->
	case containsName(RFName, RLName, AB) of
		true -> 
			Person = findTupleByName(FName, LName, AB),
			case Person of
				false -> {error, {contact_not_exists, "Nie ma takiego kontaktu w bazie"}};
				_ ->
					case isRelated(Person, Rel, RFName, RLName) of
						true -> {error, {contact_has_relation, "Kontakt juz ma te relacje"}};
						false -> lists:keyreplace(joinName(FName, LName), #person.name, AB, Person#person{relations = [ #relation{name = joinName(RFName, RLName), relname = Rel} | Person#person.relations]})
					end 
			end;
		false -> {error, {rel_contact_not_exists, "Nie ma takiego kontaktu w bazie"}}
	end.

removeRelation(FName, LName, Rel, RFName, RLName, AB) ->
	case containsName(RFName, RLName, AB) of
		true -> 
			Person = findTupleByName(FName, LName, AB),
			case Person of
				false -> {error, {contact_not_exists, "Nie ma takiego kontaktu w bazie"}};
				_ -> 
					RName = joinName(RFName, RLName),
					lists:keyreplace(joinName(FName, LName), #person.name, AB, Person#person{relations = lists:filter(fun(X) when X == #relation{name=RName, relname = Rel} -> false; (_) -> true end, Person#person.relations )}) 
			end;
		false -> {error, {rel_contact_not_exists, "Nie ma takiego kontaktu w bazie"}}
	end.

getRelations(FName, LName, AB) ->
	Person = findTupleByName(FName, LName, AB),
	case Person of 
		false -> {error, {contact_not_exists, "Nie ma takiego kontaktu w bazie"}};
		_ -> Person#person.relations
	end.

	

%% support functions

% containsName(_, []) -> false;
% containsName(Name, [H|T]) ->
% 	case string:equals(Name, H#person.name) of
% 		true -> true;
% 		_ 	 -> containsName(Name, T)
% 	end.

joinName(FName, LName) -> string:join([FName, LName], " ").
containsName(FName, LName, AB) -> containsName(joinName(FName, LName), AB).
containsName(Name, AB) -> lists:any(fun(#person{name = HName}) -> string:equal(Name, HName) end, AB).
containsMail(Mail, AB) -> lists:any(fun(#person{mails = Mails}) -> lists:member(Mail, Mails) end, AB).
containsPhone(Phone, AB) -> lists:any(fun(#person{phones = Phones}) -> lists:member(Phone, Phones) end, AB).
isRelated(Person, Rel, RFName, RLName) ->
	isRelated(Person, Rel, joinName(RFName, RLName)).
isRelated(#person{relations=Relations}, Rel, RName) ->
	lists:member(#relation{relname=Rel, name=RName}, Relations).
%%containsRelation(FName, LName, Rel, RFName, RLName, AB) ->
%%	containsRelation(joinName(FName, LName), Rel, RFName, RLName, AB).
%%containsRelation(Name, Rel, RFName, RLName, AB) ->
%%	containsRelation(Name, Rel, joinName(RFName, RLName), AB).
%%containsRelation(Name, Rel, RName, AB) ->
%%	Person = findTupleByName(Name, AB),
%%	case Person of 
%%		false -> {error, {contact_not_exists, "Nie ma takiego kontaktu w bazie"}};
%%		_ -> lists:member(#relation{name=RName, relname=Rel}, Person#person.relations)
%%	end.

findTupleByName(Name, AB) -> lists:keyfind(Name, #person.name, AB).
findTupleByName(FName, LName, AB) -> findTupleByName(joinName(FName, LName), AB).

findTupleByEmail(Mail, [H|T]) ->
	case lists:member(Mail, H#person.mails) of
		true -> H;
		_ -> findTupleByEmail(Mail, T)
	end; 
findTupleByEmail(_, []) -> {error, {mail_not_exists, "Nie ma takiego adresu e-mail w bazie"}}.

findTupleByPhone(Phone, [H|T]) ->
	case lists:member(Phone, H#person.phones) of
		true -> H;
		_ -> findTupleByPhone(Phone, T)
	end;
findTupleByPhone(_, []) -> {error, {phone_not_exists, "Nie ma takiego telefonu w bazie"}}.
