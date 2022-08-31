-module(db_distr).
-import(mf, [index_of/3]).
-include("db_record.hrl").

-export([start/0, loop/1, stop/0, insert/3, where_is/1]).
-export([remove/1, located_at/1, check_server/0]).
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
	[H|_] = nodes(),
	case is_pid(whereis(server)) of
		false -> self() ! rpc:call(H, db_distr, insert, [Name, Location, Company]);
		true ->	server ! {insert, Name, Location, Company, self()}
	end,
	receive
		Response -> Response
	end.

where_is(Name) ->
	[H|_] = nodes(),
	case is_pid(whereis(server)) of
		false -> self() ! rpc:call(H, db_distr, where_is, [Name]);
		true -> server ! {where_is, Name, self()}
	end,
	receive
		Response -> Response
	end.

remove(Name) ->
	[H|_] = nodes(),
	case is_pid(whereis(server)) of
		false -> self() ! rpc:call(H, db_distr, remove, [Name]);
		true ->	server ! {remove, Name, self()}
	end,
	receive
		Response -> Response
	end.

located_at(Location) ->
	[H|_] = nodes(),
	case is_pid(whereis(server)) of
		false -> self() ! rpc:call(H, db_distr, located_at, [Location]);
		true ->	server ! {located_at, Location, self()}
	end,
	receive
		Response -> Response
	end.
working_at(Company) ->
	[H|_] = nodes(),
	case is_pid(whereis(server)) of
		false -> self() ! rpc:call(H, db_distr, working_at, [Company]);
		true -> server ! {working_at, Company, self()}
	end,
	receive
		Response -> Response
	end.

all_names() ->
	ct:print("Pid: ~p, Server: ~p Node: ~p, Nodes: ~p~n",[self(), server, node(), nodes()]),
	ct:print("ALive: ~p, Porcess: ~p~n",[is_alive(), whereis(server)]),
	%%node() ! {all_names, self()},
	Nn = nodes(),
	ct:print("~p~n", [{is_pid(whereis(server)), length(Nn) == 0}]),
	%%case is_pid(whereis(server)) of
	case string:equal(string:substr(atom_to_list(node()), 1, 6), "server") of
		true -> 
			ct:print("It is true for name server~n", []),
			case is_pid(whereis(server)) of
				true -> 
					ct:print("True for serv~n", []),
					server ! {all_names, self()};
				false ->
					ct:print("False for serv~n", []), 
					self() ! none
			end;
		false ->
			[H|_] = nodes(),
			ct:print("False for name server and H: ~p~n", [H]),
			self() ! rpc:call(H, db_distr, all_names, [])
	end,

	receive
		Response -> 
			ct:print("Got response: ~p~n", [Response]),
			Response
	end.

all_locations() ->
	[H|_] = nodes(),
	case is_pid(whereis(server)) of
		false -> self() ! rpc:call(H, db_distr, all_locations, []);
		true ->	server ! {all_locations, self()}
	end,
	receive
		Response -> Response
	end.

get_all() ->
	server ! {get_all, self()},
	receive
		Response -> Response
	end.

check_server() ->
	ct:print("Is alive: ~p~n",[is_process_alive(whereis(server))]).
