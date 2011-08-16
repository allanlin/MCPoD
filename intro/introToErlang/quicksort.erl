-module(quicksort).
-export([sort/1]).

sort([]) ->
  [];
sort([X]) ->
  [X];
sort([H|T]) ->
  sort([L || L <- T, L < H])
  ++
  [H]
  ++
  sort([U || U <- T, U > H]).
