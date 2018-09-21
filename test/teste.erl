-module(teste).

-export([send_rec/0,
	doSendRec/0]).

send_rec() ->
	timer:apply_after(2000, ?MODULE, doSendRec, []).

doSendRec() ->
	[{any, N} ! atomFromErl || N <- nodes(hidden)],
	RetCode = receive 
		atomFromJS -> 0;
		_ -> 2
	after
		5000 -> 3
	end,

	init:stop(RetCode).
