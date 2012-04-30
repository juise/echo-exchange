-module(exchange_client).
-author('Petrovsky Alexander, <askjuise@gmail.com>').

-behaviour(gen_server).

-export([add/0, add/1, select/0, select/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).

-define(URL, "/api/v1/").

-define(NAMES, ["APPL", "MSFT", "ORCL", "YHOO", "ECHO", "AMZN", "YNDX", "GOOG"]).
-define(SCALES, ["minute", "hour", "day", "week", "month"]).
-define(ARGS, [{all}, {all, scale()}, {name, name()}, {name, name(), scale()}, {time, datetime(), datetime()}, {time, datetime(), datetime(), scale()}, {name, time, name(), datetime(), datetime()}, {name, time, name(), datetime(), datetime(), scale()}]).



select() ->
	select("127.0.0.1:8888").

select([Host]) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, "http://"++atom_to_list(Host), []),
	select_(args()),
	init:stop().

add() ->
	add("127.0.0.1:8888").

add([Host]) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, "http://"++atom_to_list(Host), []),
	Count = crypto:rand_uniform(1,100),
	[add_() || _ <- lists:seq(1, Count)],
	init:stop().

init(Host) ->
	self() ! init_inets,
	{ok, Host}.


%% Select name from names list
name() ->
	Names = dict:from_list(lists:zip(lists:seq(1, length(?NAMES)), ?NAMES)),
	Key = crypto:rand_uniform(1, length(?NAMES)),
	dict:fetch(Key, Names).

%% Gererate date time
datetime() ->
	{{Y, _M, _D}, {_H, _Mn, _S}} = calendar:local_time(),
	M = crypto:rand_uniform(1,12),
	D = crypto:rand_uniform(1,31),
	H = crypto:rand_uniform(1,24),
	Mn = crypto:rand_uniform(1,60),
	S = crypto:rand_uniform(1,60),
	dh_date:format("Y-m-dTH:i:s", {{Y, M, D}, {H, Mn, S}}).

%% Generate price or value
price_or_value() ->
	integer_to_list(crypto:rand_uniform(1,1000) div crypto:rand_uniform(1,10)).

%% Select scale from scales list
scale() ->
	Scales = dict:from_list(lists:zip(lists:seq(1, length(?SCALES)), ?SCALES)),
	Key = crypto:rand_uniform(1,length(?SCALES)),
	dict:fetch(Key, Scales).

%% Select arguments (criteria) from aeguments list
args() ->
	Args = dict:from_list(lists:zip(lists:seq(1, length(?ARGS)), ?ARGS)),
	Key = crypto:rand_uniform(1, (length(?ARGS))),
	dict:fetch(Key, Args).


%% Parse and print selected results
parse({Code, Message, []}) ->
	io:format("~w, ~s~n", [Code, Message]);

parse({_Code, _Message, Body}) ->
	{ok, {array, Records}} = json2:decode_string(Body),
	[print(Record) || Record <- Records].

%% Print selected results
print({struct, [{_Name, Name}, {_Time, Time}, {_Price, Price}, {_Value, Value}]}) ->
	io:format("~s, ~s, ~4w, ~4w~n", [Name, Time, Price, Value]);

print({struct, [{_Name, Name}, {_Time, Time}, {_OpenPrice, OpenPrice}, {_ClosePrice, ClosePrice}, {_MinPrice, MinPrice}, {_MaxPrice, MaxPrice}, {_Value, Value}]}) ->
	io:format("~s, ~s, ~4w, ~4w, ~4w, ~4w, ~4w~n", [Name, Time, OpenPrice, ClosePrice, MinPrice, MaxPrice, Value]).


select_(Args) ->
	{Code, Message, Body} = gen_server:call(?MODULE, Args),
	parse({Code, Message, Body}),
	io:format("Select criteria: ~p~n", [Args]).

add_() ->
	timer:sleep(1),
	{Code, Message, Body} = gen_server:call(?MODULE, {add, name(), datetime(), price_or_value(), price_or_value()}),
	io:format("~w, ~s - ~s~n", [Code, Message, Body]).


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
	{reply, {400, "Bad request", []}, State}.


handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(init_inets, State) ->
	inets:start(),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

