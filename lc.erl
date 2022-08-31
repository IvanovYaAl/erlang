-module(lc).
-export([atoms/1, atoms_ints/1]).
-export([keysearch/2, join/2, diff/2]).

atoms(List) ->
    atoms_(List, []).

%% One more variant
%% atoms(List) -> [X || X <- List, is_atom(X)].

atoms_([], Res) -> Res;
atoms_(List, Res) ->
    [H|T] = List,
    case is_atom(H) of
        true -> 
            Res_ = Res ++ [H],
            atoms_(T, Res_);
        false -> 
            atoms_(T, Res)
    end.


atoms_ints(List) ->
    atoms_ints_(List, []).

%% One more variant
%% atoms_ints(List) -> [X || X <- List, is_atom(X) or is_integer(X)].

atoms_ints_([], Res) -> Res;
atoms_ints_(List, Res) ->
    [H|T] = List,
    case (is_atom(H) or is_integer(H)) of
        true -> 
            Res_ = Res ++ [H],
            atoms_ints_(T, Res_);
        false -> 
            atoms_ints_(T, Res)
    end.

keysearch(Key, TupleList) ->
    [{ListsKey, Val} || {ListsKey, Val} <- TupleList, Key == ListsKey].

join(List1, List2) ->
    [X || X <- List1, lists:member(X, List2)].

diff(List1, List2) ->
    [X || X <- List1, not lists:member(X, List2)].