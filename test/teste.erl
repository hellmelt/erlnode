-module(teste).

-export([send_rec_delay/1,
	send_rec/1,
	send_rec/2,
	reg_rec_send/1]).

send_rec_delay(ArgList) ->
	timer:apply_after(250, ?MODULE, send_rec, ArgList).

send_rec([Node, Suffix]) ->
	send_rec(Node, Suffix).

send_rec(NodeStr, Suffix) ->
	Node = list_to_atom(NodeStr),
	RemoteName = list_to_atom(Suffix),
	{RemoteName, Node} ! list_to_atom("atomFromErl" ++ Suffix),
	ExpectedAtom = list_to_atom("atomFromJS" ++ Suffix),
	RetCode1 = receive 
		ExpectedAtom -> 0;
		_ -> 1
	after
		5000 -> 8
	end,

	{RemoteName, Node} ! {list_to_atom("atomFromErl" ++ Suffix), "StringFromErl" ++ Suffix, 42 + list_to_integer(Suffix)},
	ExpectedTerm = {list_to_atom("atomFromJS" ++ Suffix), "StringFromJS" ++ Suffix, 142 + list_to_integer(Suffix)},
	RetCode2 = receive
		ExpectedTerm -> 0;
		_ -> 2
	after
		5000 -> 16
	end,

	init:stop(RetCode1 + RetCode2).

reg_rec_send([NodeStr, Suffix]) ->
	Node = list_to_atom(NodeStr),
	RemoteName = list_to_atom(Suffix),
	register(list_to_atom("testprocess" ++ Suffix), self()),
	ExpectedAtom = list_to_atom("atomFromJS" ++ Suffix),
	RetCode = receive
		ExpectedAtom ->
			{RemoteName, Node} ! list_to_atom("atomFromErl2" ++ Suffix),
			0;
		_ -> 2
	after
		5000 -> 3
	end,

	init:stop(RetCode).
