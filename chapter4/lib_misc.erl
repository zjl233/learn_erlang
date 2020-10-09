-module(lib_misc).

-export([count_characters/1, for/3, odds_and_evens/1]).

for(Max, Max, F) -> [F(Max)];
for(I, Max, F) -> [F(I) | for(I + 1, Max, F)].

odds_and_evens(L) -> odds_and_evens_acc(L, [], []).

odds_and_evens_acc([H | T], Odds, Evens) ->
    case H rem 2 of
        0 -> odds_and_evens_acc(T, [H | Odds], Evens);
        _ -> odds_and_evens_acc(T, Odds, [H | Evens])
    end;
odds_and_evens_acc([], Odds, Evens) ->
    [{odds, Odds}, {evens, Evens}].

count_characters(Str) -> count_characters(Str, #{}).

count_characters([H | T], #{H := N} = X) -> % 报错 H unbound
    count_characters(T, X#{H := N + 1});
count_characters([H | T], X) ->
    count_characters(T, X#{H := 1});
count_characters([], X) -> X.

