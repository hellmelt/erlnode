-module(teste).

-export([send_rec/1,
	doSendRec/1]).

send_rec([Node]) ->
	timer:apply_after(1000, ?MODULE, doSendRec, [Node]).

doSendRec(Node) ->
	{any, Node} ! atomFromErl,
	receive 
		atomFromJS -> done 
	end,

	init:stop().
