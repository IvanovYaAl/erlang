-module(mf).
-export([setnth/3, for/3, index_of/2]).

%% Change element in the list
%% setnth(Index, List, NewElement)

setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

%% Loop For(IndexStart, IndexEnd, Function)

for(N, N, F) -> [F()];
for(I, N, F) -> [F() | for(I+1, N, F)].

%% Return index of element in the list

index_of(Item, L) -> index_of(Item, L, 1).

index_of(_, [], _) -> undefined;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

