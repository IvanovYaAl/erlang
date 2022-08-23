-module(star).
-export([start/1, send_msg/4, loop/0]).

start(N) ->
	{spawn(star, loop, []),
	for(1, N, fun() -> spawn(star, loop, []) end)}.
%%	PidMain ! {start, L, 0, M}.

send_msg(Main, L, _, N) ->
	Main ! {start, L, 0, N}.

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
		exit(normal)
	after 
	100 ->
		io:format("Main (~p) procces is dead~n", [self()]),
		exit(normal)
end.

for(N, N, F) -> [F()];
for(I, N, F) -> [F()|for(I+1, N, F)].
