-module(teste).

-export([send_rec/0,
	doSendRec/0,
	doSendRec/1,
	reg_rec_send/0]).

send_rec() ->
	timer:apply_after(2000, ?MODULE, doSendRec, []).

doSendRec() ->
	[{any, N} ! atomFromErl || N <- nodes(hidden)],
	RetCode1 = receive 
		atomFromJS -> 0;
		_ -> 1
	after
		5000 -> 8
	end,

	[{any, N} ! {atomFromErl, "StringFromErl", 42} || N <- nodes(hidden)],
	RetCode2 = receive
		{atomFromJS, "StringFromJS", 43} -> 0;
		_ -> 2
	after
		5000 -> 16
	end,

	init:stop(RetCode1 + RetCode2).

doSendRec([Node]) ->
	{any, Node} ! atomFromErl,
	RetCode1 = receive 
		atomFromJS -> 0;
		_ -> 1
	after
		5000 -> 8
	end,

	[{any, N} ! {atomFromErl, "StringFromErl", 42} || N <- nodes(hidden)],
	RetCode2 = receive
		{atomFromJS, "StringFromJS", 43} -> 0;
		_ -> 2
	after
		5000 -> 16
	end,

	init:stop(RetCode1 + RetCode2).

reg_rec_send() ->
	register(testprocess, self()),
	RetCode = receive
		atomFromJS2 ->
			[{any, N} ! atomFromErl2 || N <- nodes(hidden)],
			0;
		_ -> 2
	after
		5000 -> 3
	end,

	init:stop(RetCode).
