-module(exchange_client).
-author('Petrovsky Alexander, <askjuise@gmail.com>').

-behaviour(gen_server).

-export([start_link/0, add/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).

-define(NAMES, ["APPL", "MSFT", "ORCL", "YHOO", "ECHO", "AMZN", "YNDX", "GOOG"]).


start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	self() ! init_inets,
	{ok, []}.


get_name() ->
	Names = dict:from_list(lists:zip(lists:seq(1, length(?NAMES)), ?NAMES)),
	Key = random:uniform(length(?NAMES)),
	dict:fetch(Key, Names).

get_time() ->
	{{Y, _M, _D}, {_H, _Mn, _S}} = calendar:local_time(),
	M = random:uniform(12),
	D = random:uniform(31),
	H = random:uniform(24),
	Mn = random:uniform(60),
	S = random:uniform(60),
	dh_date:format("Y-m-dTH:i:s", {{Y, M, D}, {H, Mn, S}}).

get_price() ->
	integer_to_list(random:uniform(1000) div random:uniform(10)).

get_value() ->
	integer_to_list(random:uniform(1000) div random:uniform(10)).

add(Count) ->
	[add() || _ <- lists:seq(1, Count)].

add() ->
	{{_Protocol, _Code, Message}, _, _} = gen_server:call(?MODULE, {add, get_name(), get_time(), get_price(), get_value()}),
	timer:sleep(1),
	Message.

handle_call({add, Name, Time, Price, Value}, _From, State) ->
	Accept = "*/*",
	Data = "name="++Name++"&time="++Time++"&price="++Price++"&value="++Value,
	{ok, Reply} = httpc:request(post, {"http://127.0.0.1:8888/api/v1/add", [], Accept, Data}, [], []),
	{reply, Reply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(init_inets, State) ->
	inets:start(),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

