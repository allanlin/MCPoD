-module(quicksort2).
-export([sort/1]).

sort([]) -> [];
sort([H|T]) ->
  [Less,More] = partition(H,T,[],[]),
  sort(Less) ++ [H] ++ sort(More).

partition(_, [] , Less, More) -> [Less,More];
partition(Pivot, [H|T] , Less, More) ->
  if Pivot =< H -> partition( Pivot, T, [H|Less], More);
     Pivot >  H -> partition( Pivot, T, Less, [H|More])
  end.
