-module(calculator).

-export([add/1]).

add(NumberList) when is_list(NumberList) ->
    add(0, NumberList);

add(_) -> {badarg, notAList}.

add(Acc, []) ->
    Acc;

add(Acc, [Number | Rest]) when is_number(Number) ->
    add(Acc + Number, Rest);

add(_, _) -> {badarg, notANumber}.
