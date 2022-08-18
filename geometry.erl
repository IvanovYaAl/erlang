-module(geometry).
-export([area/1]).

area({rectangle, Width, Ht}) -> 2 * Width + 2 * Ht;
area({square, X}) -> X * 4;
area({triangle, A, B, C}) -> A + B + C;
area({circle, R}) -> 3.14159 * R * 2.
