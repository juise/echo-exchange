-module(exchange_client).
-author('Petrovsky Alexander, <askjuise@gmail.com>').

-behaviour(gen_server).

-export([start_link/1, select/1, add/0]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).

-record(paper, {name, time, price, value}).

-define(URL, "/api/v1/").

-define(NAMES, ["APPL", "MSFT", "ORCL", "YHOO", "ECHO", "AMZN", "YNDX", "GOOG"]).


start_link(Host) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Host, []).

init(Host) ->
	self() ! init_inets,
	{ok, Host}.


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

get_price_or_value() ->
	integer_to_list(random:uniform(1000) div random:uniform(10)).

paper({struct, [{_Name, Name}, {_Time, Time}, {_Price, Price}, {_Value, Value}]}) ->
	#paper{name=Name, time=Time, price=Price, value=Value}.


select(all) ->
	{Code, Message, Body} = gen_server:call(?MODULE, {all}),
	{ok, {array, Records}} = json2:decode_string(Body),
	[io:format("~p~n", [paper(Record)]) || Record <- Records].

add() ->
	Count = random:uniform(10000),
	[add_() || _ <- lists:seq(1, Count)],
	io:format("Amount of requests ~w~n", [Count]).

add_() ->
	timer:sleep(1),
	{Code, Message, Body} = gen_server:call(?MODULE, {add, get_name(), get_time(), get_price_or_value(), get_price_or_value()}),
	io:format("~w, ~s ~s~n", [Code, Message, Body]).


handle_call({all}, _From, State) ->
	{ok, {{_Version, Code, Message}, _Headers, Body}} = httpc:request(get, {State++?URL++"all", []}, [], []),
	{reply, {Code, Message, Body}, State};

handle_call({all, Scale}, _From, State) ->
	{ok, {{_Version, Code, Message}, _Headers, Body}} = httpc:request(get, {State++?URL++"all"++"/"++Scale, []}, [], []),
	{reply, {Code, Message, Body}, State};

handle_call({name, Name}, _From, State) ->
	{ok, {{_Version, Code, Message}, _Headers, Body}} = httpc:request(get, {State++?URL++Name, []}, [], []),
	{reply, {Code, Message, Body}, State};

handle_call({name, Name, Scale}, _From, State) ->
	{ok, {{_Version, Code, Message}, _Headers, Body}} = httpc:request(get, {State++?URL++Name++"/"++Scale, []}, [], []),
	{reply, {Code, Message, Body}, State};

handle_call({time, T1, T2}, _From, State) ->
	{ok, {{_Version, Code, Message}, _Headers, Body}} = httpc:request(get, {State++?URL++T1++"/"++T2, []}, [], []),
	{reply, {Code, Message, Body}, State};

handle_call({time, T1, T2, Scale}, _From, State) ->
	{ok, {{_Version, Code, Message}, _Headers, Body}} = httpc:request(get, {State++?URL++T1++"/"++T2++"/"++Scale, []}, [], []),
	{reply, {Code, Message, Body}, State};

handle_call({name, time, Name, T1, T2}, _From, State) ->
	{ok, {{_Version, Code, Message}, _Headers, Body}} = httpc:request(get, {State++?URL++Name++"/"++T1++"/"++T2, []}, [], []),
	{reply, {Code, Message, Body}, State};

handle_call({name, time, Name, T1, T2, Scale}, _From, State) ->
	{ok, {{_Version, Code, Message}, _Headers, Body}} = httpc:request(get, {State++?URL++Name++"/"++T1++"/"++T2++"/"++Scale, []}, [], []),
	{reply, {Code, Message, Body}, State};

handle_call({add, Name, Time, Price, Value}, _From, State) ->
	Accept = "*/*",
	Data = "name="++Name++"&time="++Time++"&price="++Price++"&value="++Value,
	{ok, {{_Version, Code, Message}, _Headers, Body}} = httpc:request(post, {State++?URL++"add", [], Accept, Data}, [], []),
	{reply, {Code, Message, Body}, State};

handle_call(_Msg, _From, State) ->
	{reply, error, State}.


handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(init_inets, State) ->
	inets:start(),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

