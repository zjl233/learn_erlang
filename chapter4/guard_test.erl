-module(guard_test).
-export([f/1, g/1]).

f(X) when (X == 0) or (1 / X > 2) -> ok.

g(X) when(X == 0) orelse(1 / X > 2) -> ok.