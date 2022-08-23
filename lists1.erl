-module(lists1).
-export([min/1, max/1, min_max/1]).

min([]) -> [];
min([H|T]) -> min(H, T).
min(Min, [H|T]) ->
	case Min < H of
		true -> min(Min, T);
		false -> min(H, T)
	end;
min(Min, []) -> Min.



max([]) -> [];
max([H|T]) -> max(H, T).
max(Max, [H|T]) ->
	case Max > H of
		true -> max(Max, T);
		false -> max(H, T)
	end;
max(Max, []) -> Max.


min_max(X) -> {min(X), max(X)}.
