%%%-------------------------------------------------------------------
%%% @author rwalski
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. gru 2016 3:06 PM
%%%-------------------------------------------------------------------
-module(records).
-author("rwalski").

%% API
%%-export([]).

-compile(export_all).

-record(user, {id, name, group, age}).


%% use pattern matching to filter
admin_panel(#user{name=Name, group=admin}) ->
  Name ++ " is allowed!";
admin_panel(#user{name=Name}) ->
  Name ++ " is not allowed".

%% can extend user without problem
adult_section(U = #user{}) when U#user.age >= 18 ->
%% Show stuff that can't be written in such a text
  allowed;
adult_section(_) ->
%% redirect to sesame street site
  forbidden.

yo() ->
  records:adult_section(#user{id=1, name="ferd", group=admin, age=96}).