%%%-------------------------------------------------------------------
%%% @author anhj03
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Apr 2019 19:03
%%%-------------------------------------------------------------------
-module(test_rpc_server).
-author("anhj03").

%% API
-export([run_test/1]).

run_test([NodeStr]) ->
    Node = list_to_atom(NodeStr),
    Module = './test/helpers/greetings',

    RetCode1 = case rpc:call(Node, Module, sayHello, ["Anders"]) of
                   "Hello Anders, JS here!" -> 0;
                   _ -> 1
               end,

    rpc:cast(Node, Module, setGlobalTime, ["Evening"]),

    RetCode2 = case rpc:call(Node, Module, sayGood, []) of
                   "GoodEvening, JS here!" -> 0;
                   _ -> 2
               end,

    rpc:call(Node, Module, quit, 10000),

    init:stop(RetCode1 + RetCode2).
