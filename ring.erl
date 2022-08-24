-module(ring).
-export([start/1, loop/0, send_msg/3]).

start(N) ->
	for(1, N, fun() -> spawn(ring, loop, []) end).

send_msg(L, _, N) -> 
	[H|T] = L,
	H ! {list, T, 1, N, L}.

loop() -> 
	receive
		{list, Lis, N, M, L} ->
			[H|T] = Lis,
			io:format("I (~p) got message #~p~n", [self(), N]), 
			case length(T) == 0 of
				false -> H ! {list, T, N, M, L};
				true -> H ! {empty, T, N, M, L}
			end,
			loop();
		{empty, _, N, M, L} ->
			N1 = N + 1,
			case N1 > M of 
				false -> 
					io:format("I (~p) got message #~p~n", [self(), N]),
					[H|T] = L,
					H ! {list, T, N1, M, L};
				true ->
					io:format("I (~p) get last message #~p~n", [self(), N]),
					[H|T] = L,
					H ! {dead, T, {die}}
			end,
			loop();
		{dead, {die}} ->
			exit(normal);
		{dead, L, {die}} ->
			[H|T] = L,
			case length(T) == 0 of
				false -> H ! {dead, T, {die}};
				true -> io:format("The END~n Messages stop sends~n", []),
					H ! {dead, {die}}
			end,
			exit(normal)
	end. 

for(N, N, F) -> [F()];
for(I, N, F) -> [F() | for(I+1, N, F)].
