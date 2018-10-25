-module(accu).

-behaviour(gen_server).

-export([start_link/0]).

-export([add/1, subtract/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

add(Number) ->
    gen_server:call(?MODULE, {add, Number}).

subtract(Number) ->
    gen_server:call(?MODULE, {subtract, Number}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, 0}.

handle_call({add, Number}, _From, Accumulator) ->
    NewAccumulator = Accumulator + Number,
    {reply, NewAccumulator, NewAccumulator};

handle_call({subtract, Number}, _From, Accumulator) ->
    NewAccumulator = Accumulator - Number,
    {reply, NewAccumulator, NewAccumulator};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({add, Number}, Accumulator) ->
    NewAccumulator = Accumulator + Number,
    {noreply, NewAccumulator};

handle_cast({subtract, Number}, Accumulator) ->
    NewAccumulator = Accumulator - Number,
    {noreply, NewAccumulator};

handle_cast(stop, Accumulator) ->
    {stop, castRequest, Accumulator};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(castRequest, _State) ->
    init:stop(0);

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions