-module(geometry).
-export([area/1]).

area({square, Side}) -> Side * Side;
area({rectangle, Width, Height}) -> Width * Height.


