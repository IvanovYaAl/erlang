-module(ms).
-import(mf, [setnth/3, for/3, index_of/2]).
-export([start/1, to_slave/2, msLoop/1, loop/0]).

start(N) ->
	case is_pid(whereis(master)) of
		true -> unregister(master);
		false -> master_is_undefind
	end,
	register(master, spawn(fun() -> master_start(N) end)).

master_start(N) ->
	process_flag(trap_exit, true),
	L = mf:for(1, N, fun() -> spawn_link(fun() -> loop() end) end),
	io:format("Master: ~p~nSlaves: ~p~n", [self(), L]),
	msLoop(L).

to_slave(Message, N) ->
	master ! {Message, N}.
	
msLoop(L) ->
	receive
		showSlaves -> io:format("Slaves: ~p~n", [L]), msLoop(L);

		{Message, N} ->
		Slave = lists:nth(N, L),
		got_message(Message, N, Slave),
		msLoop(L);

		{'EXIT', Pid, _} ->
			io:format("OMG someone died.. It was: ~p~n",[Pid]),
			N = mf:index_of(Pid, L),
			self() ! {rein, N, N},
			msLoop(L);

		{rein, N, N} -> 
		NewPid = spawn_link(fun() -> loop() end),
		L1 = mf:setnth(N, L, NewPid),
%%		io:format("New slaves: ~p~n", [L1]),
		msLoop(L1) 
	end.


got_message(die, N, Pid) -> Pid ! {die, N, N};
got_message(Message, N, Pid) -> Pid ! {Message, N}.

loop() -> 
	receive
		{Message, N} -> io:format("Slave ~p got message ~p~n", [N, Message]),
		loop();
	
		{die, N, N} -> 
		master ! {rein, N, N},
		io:format("Master restarting dead slave ~p~n", [N]),
		exit(normal);

		die -> exit(killed)
	end.
