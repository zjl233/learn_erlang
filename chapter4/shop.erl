-module(shop).
-export([total/1]).



% shop() ->
%     Items = [{oranges, 4}, {newspaper, 5}, {apple, 10}, {pears, 6}],
%     ok.

cost(oranges) -> 5;
cost(newspaper) -> 10;
cost(apple) -> 10;
cost(pears) -> 8.

% total([{What, N}|T]) ->
%     cost(What) * N + total(T);
% total([]) -> 0.

total(Items) ->
    lists:sum([cost(What) * N || {What, N} <- Items]).