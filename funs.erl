-module(funs).
-export([llen/1, min/1, rail/2]).
-export([rail/2, all_even/1, first/2]).
-export([mk_adder/1]).

llen(Ls) -> lists:map(fun(Y) -> length(Y) end, Ls).

min(Ls) -> lists:foldl(fun(X, Y) -> min(X, Y) end, lists:last(Ls), Ls).

rail(Pred, List) ->
    lists:foldl(fun(X, Y)->
        case Y of
            true -> Pred(X);
            false -> false
        end
            end, true, List).

all_even(List) ->
    rail(fun(X) -> (X rem 2) =:= 0 end, List).

first(Pred, List) ->
    lists:foldl(fun(X, Y) ->
        case Y of
            false -> case Pred(X) of
                        true -> X;
                        false -> false
                    end;
            M -> M
        end
            end, false, List).

mk_adder(Const) -> fun(X) -> Const + X end.
%%llen(Lis) ->
%%    llen1(Lis, []).
%%llen1([],Res) -> 
%%   Res;
%%llen1(Lis, Res) ->
%%    [H|T] = Lis,
%%   _Res = Res ++ [length(H)],
%%    llen1(T, _Res).