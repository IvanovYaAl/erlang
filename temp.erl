-module(temp).
-export([f2c/1, c2f/1, convert/1]).

f2c(C) -> 32 + C*9/5.

c2f(F) -> (F-32) * 5/9.

convert({c, C}) -> {f, 32 + C*9/5};
convert({f, F}) -> {c, (F-32) * 5/9}. 
