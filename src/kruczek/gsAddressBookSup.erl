-module(gsAddressBookSup).
-behaviour(supervisor).

-export([start_link/0, start_link/1, init/1]).

start_link() ->
	start_link(addressBook:createAddressBook()).

start_link(InitVal) ->
	supervisor:start_link({local, ab_sup}, ?MODULE, InitVal).

init(InitVal) ->
	{ok, {
		{one_for_all, 2, 2000},
		[{ab_server, {gsAddressBook, start_link, [InitVal]}, permanent, brutal_kill, worker, [gsAddressBook]}]
	}}.

