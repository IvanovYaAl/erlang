-module(mf).
-export([setnth/3, for/3]).

%% Change element in the list
%% setnth(Index, List, NewElement)

setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

%% Loop For(IndexStart, IndexEnd, Function)

for(N, N, F) -> [F()];
for(I, N, F) -> [F() | for(I+1, N, F)].
