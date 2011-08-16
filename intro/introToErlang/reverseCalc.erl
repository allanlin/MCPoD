-module(reverseCalc).
-export([parse/1,rev/2]).

parse(S) ->
  X = string:tokens(S," "),
  rev([],X).

read(N) ->
  case string:to_integer(N) of
    {error,not_a_list} -> N;
    {F,_} -> F
  end.

rev([A1|[A2|R1]], ["+"|R2]) -> rev([read(A1)+read(A2)|R1],R2);
rev([A1|[A2|R1]], ["-"|R2]) -> rev([read(A1)-read(A2)|R1],R2);
rev([A1|[A2|R1]], ["/"|R2]) -> rev([read(A1)/read(A2)|R1],R2);
rev([A1|[A2|R1]], ["*"|R2]) -> rev([read(A1)*read(A2)|R1],R2);
rev(Sol, [A|B])             -> rev(Sol++[A],B);
rev(Sol, [])                -> Sol.



