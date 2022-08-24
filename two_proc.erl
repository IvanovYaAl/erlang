-module(two_proc).
-export([start/1, loop/0, send_msg/4]).

start(M) ->
	{spawn(two_proc, loop, []),
	spawn(two_proc, loop, [])}.

send_msg(Pid1, Pid2, _, N) ->
	Pid1 ! {Pid2, 1, N},
	Pid2 ! {Pid1, 1, N}.

loop() ->
	receive
		{Pid1, N, M} when is_pid(Pid1) ->
		case N > M of
			false ->
				io:format("Message #~p~n", [N]), 
				Pid1 ! {self(), N + 1, M};
			true -> 
				io:format("Process ~p is dead~n", [self()]),
				exit(normal)
		end,
		loop();
		{Pid2, N, M} when is_pid(Pid2) ->
		case N > M of 
			false ->
				io:format("Message #~p~n", [N]), 
				Pid2 ! {self(), N + 1, M};
			true -> 
				io:format("Process ~p is dead~n", [self()]),
				exit(normal)
		end,
		loop();
		Other -> io:format("WRONG ~p!!! ~n", [Other]),
		loop()
	end.
