-module(unitTests).
-export([makeTests/0]).

makeTests() ->
	AB = addressBook:createAddressBook(),
	io:format("~p~n~n", [AB]),
	AB1 = addressBook:addContact("Jan", "Kowalski", AB),
	io:format("~p~n~n", [AB1]),
	AB2 = addressBook:addEmail("Jan", "Kowalski", "jasiek@kowal.pl", AB1),
	io:format("~p~n~n", [AB2]),
	AB3 = addressBook:addPhone("Jan", "Kowalski", "0700846544", AB2),
	io:format("~p~n~n", [AB3]),
	AB4 = addressBook:addContact("Zygmunt", "Stary", AB3),
	io:format("~p~n~n", [AB4]),
	AB5 = addressBook:removeContact("Zygmunt", "Stary", AB4),
	io:format("~p~n~n", [AB5]),
	AB6 = addressBook:addContact("Anna", "Wiśniewska", AB5),
	io:format("~p~n~n", [AB6]),
	AB7 = addressBook:addEmail("Anna", "Wiśniewska", "anka@mail.com.pl", AB6),
	io:format("~p~n~n", [AB7]),
	AB8 = addressBook:addPhone("Anna", "Wiśniewska", "656468549", AB7),
	io:format("~p~n~n", [AB8]),
	P1 = addressBook:findByEmail("anka@mail.com.pl", AB8),
	io:format("~p~n~n", [P1]),
	P2 = addressBook:findByPhone("656468549", AB8),
	io:format("~p~n~n", [P2]),
	Mails = addressBook:getEmails("Anna", "Wiśniewska", AB8),
	io:format("~p~n~n", [Mails]),
	Phones = addressBook:getPhones("Anna", "Wiśniewska", AB8),
	io:format("~p~n~n", [Phones]),



	ok.

