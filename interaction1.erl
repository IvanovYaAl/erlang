-module(interaction1).
-export([start/2, loop/0]).

start(N, M) ->
	L = for(1, N, fun() -> spawn(interaction1, loop, []) end),
	io:format("~p, ~p ~n", [L, M]),
	[H|T] = L,
	H ! {list, T, 0, M, L}.

loop() -> 
	receive
		{list, Lis, N, M, L} ->
			io:format("I : ~p got the message: ~p~n", [self(), N]),
			[H|T] = Lis,
			case length(T) == 0 of
				false -> H ! {list, T, N, M, L};
				true -> H ! {empty, T, N, M, L}
			end,
			loop();
		{empty, Lis, N, M, L} ->
			io:format("End of circle: ~p Number: ~p~n", [Lis, N]),
			N1 = N + 1,
			case N1 > M of 
				false -> 
					[H|T] = L,
					H ! {list, T, N1, M, L};
				true ->
					[H|T] = L,
					H ! {dead, T, {die}}
			end,
			loop();
		{dead, L, {die}} ->
			[H|T] = L,
			case length(T) == 0 of
				false -> H ! {dead, T, {die}};
				true -> io:format("The END~n Messages stop sends~n", [])
			end,
			exit({die})
	end. 

for(N, N, F) -> [F()];
for(I, N, F) -> [F() | for(I+1, N, F)].
