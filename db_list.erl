-module(db_list).
-import(mf, [index_of/3]).
-export([start/0, loop/1, stop/0, insert/2, where_is/1, remove/1, located_at/1,
	all_names/0, all_locations/0]).

%% Server
start() ->
	L = [],
	Server = spawn(db_list, loop, [L]),
	register(server, Server),
	ok.

loop(L) ->
receive
	{insert, Name, Location, Pid} ->
		case lists:keymember(Name, 2, L) of 
			false -> L1 = L ++ [{user, Name, Location}],
				Pid ! ok;
			true -> L1 = L,
				Pid ! already_inserted
		end,
		loop(L1);

	{where_is, Name, Pid} ->
		Tuple = lists:keyfind(Name, 2, L),
		case Tuple == false of 
			true -> Pid ! {no_such_name, Name};
			false -> {_, _, X} = Tuple, 
				Pid ! X
		end,
		loop(L);
		
	{remove, Name, Pid} ->
		Tuple = lists:keyfind(Name, 2, L),
		case Tuple == false of
			true -> L1 = L,
				Pid ! ok;
			false -> L1 = lists:keydelete(Name, 2, L),
			Pid ! ok
		end,
		loop(L1);

	{located_at, Location, Pid} ->
		Ls = get_located(L, Location, []),
		case length(Ls) == 0 of
			true -> Pid ! none;
			false -> Pid ! Ls
		end,
		loop(L);

	{all_names, Pid} ->
		Ls = lists:ukeysort(2, L),
		Pid ! get_all_names(Ls, []),
		loop(L);

	{all_locations, Pid} ->
		Ls = lists:ukeysort(3, L),
		Pid ! get_all_locations(Ls, []),
		loop(L);

	{stop, Pid} -> io:format("Server shuted down",[]),
		Pid ! server_terminated,
		exit(normal)
end.

get_all_locations([], Locations) -> Locations;
get_all_locations(L, Locations) ->
	[H|T] = L,
	{_, _, X} = H,
	Locations1 = Locations ++ [X],
	get_all_locations(T, Locations1).

get_all_names([], Names) -> Names;
get_all_names(L, Names) ->
	[H|T] = L,
	{_, X, _} = H,
	Names1 = Names ++ [X],
	get_all_names(T, Names1).


get_located([], _, Locations) -> Locations;
get_located(L, Name, Locations) ->
	[H|T] = L,
	{_, Y, X} = H,
	case X == Name of
		true -> En1 = Locations ++ [Y], get_located(T, Name, En1);
		false -> get_located(T, Name, Locations)
	end.

stop() ->
	server ! {stop, self()},
	receive
		Response -> Response
	end.

%% Client

insert(Name, Location) ->
	server ! {insert, Name, Location, self()},
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
