-module(mathStuff).
-export([perimeter/1]).

perimeter({rectangle, Width, Ht}) -> 2 * Width + 2 * Ht;
perimeter({square, X}) -> X * 4;
perimeter({triangle, A, B, C}) -> A + B + C;
perimeter({circle, R}) -> math:pi() * R * 2.
