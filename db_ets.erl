-module(db_ets).
-import(mf, [index_of/3]).
-include("db_record.hrl").

-export([start/0, loop/1, stop/0, insert/3, where_is/1]).
-export([remove/1, located_at/1]).
-export([all_names/0, all_locations/0, working_at/1, get_all/0]).

%% Server
start() ->
	Server = spawn(fun() -> loop(ets:new(my_ets, [bag, protected, {keypos, #users.name}])) end),
	register(server, Server),
	ok.

loop(L) ->
receive
	{insert, Name, Location, Company, Pid} ->
		NewUser = #users{name=Name, location=Location, company=Company},
		Ls = lists:append(ets:match(L, #users{name=Name, location='$2', company='_'})),
		case length(Ls) == 0 of 
			true ->
				ets:insert(L, NewUser),
				Pid ! ok;
			false ->
				Pid ! already_inserted
		end,
		loop(L);

	{where_is, Name, Pid} ->
		Loc = lists:append(ets:match(L, #users{name=Name, location='$2', company='_'})),
		case length(Loc) == 0 of 
			true -> Pid ! {no_such_name, Name};
			false -> Tuple = list_to_tuple(Loc),
				{X} = Tuple, 
				Pid ! X
		end,
		loop(L);
		
	{remove, Name, Pid} ->
		ets:delete(L, Name),
		Pid ! ok,
		loop(L);

	{located_at, Location, Pid} ->
		Ls = lists:append(ets:match(L, #users{name='$1', location=Location, company='_'})),
		case length(Ls) == 0 of
			true -> Pid ! none;
			false -> Pid ! Ls
		end,
		loop(L);

	{working_at, Company, Pid} ->
		Names = lists:append(ets:match(L, #users{name='$1', location='_', company=Company})),
		case Names == [] of
			false -> Pid ! Names;
			true -> Pid ! none
		end,
		loop(L);

	{all_names, Pid} ->
		Ls = lists:merge(ets:match(L, #users{name='$1', location='_', company='_'})),
		Pid ! Ls,
		loop(L);

	{all_locations, Pid} ->
		Ls = lists:usort(lists:append(ets:match(L, #users{name='_', location='$2', company='_'}))),
		Pid ! Ls,
		loop(L);

	{get_all, Pid} ->
		Pid ! L,
		loop(L);

	{stop, Pid} -> io:format("Server shuted down",[]),
		Pid ! server_terminated,
		exit(normal)
end.

stop() ->
	server ! {stop, self()},
	receive
		Response -> Response
	end.

%% Client

insert(Name, Location, Company) ->
	server ! {insert, Name, Location, Company, self()},
	receive
		Response -> Response
	end.

where_is(Name) ->
	server ! {where_is, Name, self()},
	receive
		Response -> Response
	end.

remove(Name) ->
	server ! {remove, Name, self()},
	receive
		Response -> Response
	end.

located_at(Location) ->
	server ! {located_at, Location, self()},
	receive
		Response -> Response
	end.
working_at(Company) ->
	server ! {working_at, Company, self()},
	receive
		Response -> Response
	end.

all_names() ->
	server ! {all_names, self()},
	receive
		Response -> Response
	end.

all_locations() ->
	server ! {all_locations, self()},
	receive
		Response -> Response
	end.

get_all() ->
	server ! {get_all, self()},
	receive
		Response -> Response
	end.