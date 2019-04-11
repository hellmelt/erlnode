%%%-------------------------------------------------------------------
%%% @author anhj03
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Nov 2018 16:51
%%%-------------------------------------------------------------------
-module(test_gen_server).
-author("anhj03").

%% API
-export([run_test/1]).

run_test([NodeStr]) ->
    Node = list_to_atom(NodeStr),
    RetCode1 = case gen_server:call({accumulator, Node}, {add, 7}) of
                   7 -> 0;
                   _ -> 1
               end,

    gen_server:cast({accumulator, Node}, {add, 10}),

    RetCode2 = case gen_server:call({accumulator, Node}, {subtract, 5}) of
                   12 -> 0;
                   _ -> 2
               end,

    gen_server:cast({accumulator, Node}, {subtract, 12}),

    RetCode3 = case gen_server:call({accumulator, Node}, {add, 3}) of
                   3 -> 0;
                   _ -> 4
               end,

    init:stop(RetCode1 + RetCode2 + RetCode3).
