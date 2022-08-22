-module(ms).
-import(mf, [setnth/3, for/3]).
-export([start/1, to_slave/2, msLoop/1, loop/0]).

start(N) ->
	L = mf:for(1, N, fun() -> spawn(ms, loop, []) end),
	Master = spawn(ms, msLoop, [L]),
	case is_pid(whereis(msPid)) of
		true -> unregister(msPid);
		false -> msPid_is_undefind
	end,
	register(msPid, Master),
	io:format("Master: ~p~nSlaves: ~p~n", [Master, L]).

to_slave(Message, N) ->
	msPid ! {Message, N}.
	
msLoop(L) ->
	receive
		{Message, N} ->
		Slave = lists:nth(N, L),
		got_message(Message, N, Slave),
		msLoop(L);

		{rein, N, N} -> NewPid = spawn(ms, loop, []),
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
		msPid ! {rein, N, N},
		io:format("Master restarting dead slave ~p~n", [N]),
		loop()
	end.
