-module(time).
-export([swedish_date/0, getYear/1, getMoth/1, getDay/1]).

getYear({Y, _, _}) -> integer_to_list(Y - 2000).
getMoth({_, M, _}) -> 
	case M >= 10 of
		true -> integer_to_list(M);
		false -> integer_to_list(0) ++ integer_to_list(M)
	end.
getDay({_, _, D}) ->
	case D >= 10 of
		true -> integer_to_list(D);
		false -> integer_to_list(0) ++ integer_to_list(D)
	end.

swedish_date() -> getYear(date()) ++ getMoth(date()) ++ getDay(date()).
