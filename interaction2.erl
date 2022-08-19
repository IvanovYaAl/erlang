-module(interaction2).
-export([start/2, loop/0]).

start(N, M) ->
	PidMain = spawn(interaction2, loop, []),
	L = for(1, N, fun() -> spawn(interaction2, loop, []) end),
	io:format("New processes:~p~nAnd main ~p~n", [L, PidMain]),
	PidMain ! {start, L, 0, M}.

loop() ->
receive
	{start, L, N, M} -> 
		lists:foreach(fun(Pid) -> Pid ! {get, self(), N, M} end, L),
		loop();
	{get, Pid, N, M} ->
		io:format("I (~p) got message #~p from ~p~n", [self(), N, Pid]),
		Pid ! {get_main, self(), N, M},
		loop();
	{get_main, Pid, N, M} ->
		N1 = N + 1,
		io:format("Main got mes from: ~p #~p~n", [Pid, N1]),
		case N1 >= M of 
			false -> Pid ! {get, self(), N1, M};
			true -> Pid ! {die}
		end,
		loop();
	{die} ->
		io:format("I (~p) died...~n", [self()]),
		exit({die})
	after 
	1000 ->
		io:format("Main (~p) procces is dead~n", [self()]),
		exit({die})
end.

for(N, N, F) -> [F()];
for(I, N, F) -> [F()|for(I+1, N, F)].
