-module(teste).

-export([send_rec_delay/1,
	send_rec/1,
	reg_rec_send/1]).

send_rec_delay(NodeList) ->
	timer:apply_after(1000, ?MODULE, send_rec, NodeList).

send_rec(Node) ->
	{any, Node} ! atomFromErl,
	RetCode1 = receive 
		atomFromJS -> 0;
		_ -> 1
	after
		5000 -> 8
	end,

	{any, Node} ! {atomFromErl, "StringFromErl", 42},
	RetCode2 = receive
		{atomFromJS, "StringFromJS", 43} -> 0;
		_ -> 2
	after
		5000 -> 16
	end,

	init:stop(RetCode1 + RetCode2).

reg_rec_send([Node]) ->
	register(testprocess, self()),
	RetCode = receive
		atomFromJS2 ->
			{any, Node} ! atomFromErl2,
			0;
		_ -> 2
	after
		5000 -> 3
	end,

	init:stop(RetCode).
