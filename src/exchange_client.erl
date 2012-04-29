-module(exchange_client).
-author('Petrovsky Alexander, <askjuise@gmail.com>').

-behaviour(gen_server).

-export([start_link/1, add/0]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).

-compile(export_all).

-define(URL, "/api/v1/").

-define(NAMES, ["APPL", "MSFT", "ORCL", "YHOO", "ECHO", "AMZN", "YNDX", "GOOG"]).
-define(SCALES, ["minute", "hour", "day", "week", "month"]).
-define(ARGS, [{all}, {all, scale()}, {name, name()}, {name, name(), scale()}, {time, datetime(), datetime()}, {time, datetime(), datetime(), scale()},
		{name, time, name(), datetime(), datetime()}, {name, time, name(), datetime(), datetime(), scale()}]).


start_link(Host) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Host, []).

init(Host) ->
	self() ! init_inets,
	{ok, Host}.


name() ->
	Names = dict:from_list(lists:zip(lists:seq(1, length(?NAMES)), ?NAMES)),
	Key = random:uniform(length(?NAMES)),
	dict:fetch(Key, Names).

datetime() ->
	{{Y, _M, _D}, {_H, _Mn, _S}} = calendar:local_time(),
	M = random:uniform(12),
	D = random:uniform(31),
	H = random:uniform(24),
	Mn = random:uniform(60),
	S = random:uniform(60),
	dh_date:format("Y-m-dTH:i:s", {{Y, M, D}, {H, Mn, S}}).

price_or_value() ->
	integer_to_list(random:uniform(1000) div random:uniform(10)).

scale() ->
	Scales = dict:from_list(lists:zip(lists:seq(1, length(?SCALES)), ?SCALES)),
	Key = random:uniform(length(?SCALES)),
	dict:fetch(Key, Scales).

args() ->
	Args = dict:from_list(lists:zip(lists:seq(1, length(?ARGS)), ?ARGS)),
	Key = random:uniform(length(?ARGS)),
	dict:fetch(Key, Args).


out({struct, [{_Name, Name}, {_Time, Time}, {_Price, Price}, {_Value, Value}]}) ->
	io:format("~s, ~s, ~w, ~w~n", [Name, Time, Price, Value]);

out({struct, [{_Name, Name}, {_Time, Time}, {_OpenPrice, OpenPrice}, {_ClosePrice, ClosePrice}, {_MinPrice, MinPrice}, {_MaxPrice, MaxPrice}, {_Value, Value}]}) ->
	io:format("~s, ~s, ~w, ~w, ~w, ~w, ~w~n", [Name, Time, OpenPrice, ClosePrice, MinPrice, MaxPrice, Value]).


parse({Code, Message, []}) ->
	io:format("~w, ~s~n", [Code, Message]);

parse({Code, Message, Body}) ->
	{ok, {array, Records}} = json2:decode_string(Body),
	[out(Record) || Record <- Records],
	ok.

select() ->
%	Count = random:uniform(10),
%	[select(args()) || _ <- lists:seq(1, Count)],
%	io:format("Amount of requests ~w~n", [Count]).
	A = args(),
	select(A).


select(Args) ->
	{Code, Message, Body} = gen_server:call(?MODULE, Args),
	parse({Code, Message, Body});


%select({all}) ->
%	{_Code, _Message, Body} = gen_server:call(?MODULE, {all}),
%	{ok, {array, Records}} = json2:decode_string(Body),
%	[io:format("~s, ~s, ~w, ~w~n", paper(Record)) || Record <- Records],
%	ok;
%
%select({all, Scale}) ->
%	{_Code, _Message, Body} = gen_server:call(?MODULE, {all, Scale}),
%	{ok, {array, Records}} = json2:decode_string(Body),
%	[io:format("~s, ~s, ~w, ~w, ~w, ~w, ~w~n", multifor(Record)) || Record <- Records],
%	ok;
%
%select({name, Name}) ->
%	{_Code, _Message, Body} = gen_server:call(?MODULE, {name, Name}),
%	{ok, {array, Records}} = json2:decode_string(Body),
%	[io:format("~s, ~s, ~w, ~w~n", paper(Record)) || Record <- Records],
%	ok;
%
%select({name, Name, Scale}) ->
%	io:format("q~n"),
%	{_Code, _Message, Body} = gen_server:call(?MODULE, {name, Name, Scale}),
%	{ok, {array, Records}} = json2:decode_string(Body),
%	[io:format("~s, ~s, ~w, ~w, ~w, ~w, ~w~n", multifor(Record)) || Record <- Records],
%	ok;
%
%select({time, T1, T2}) ->
%	io:format("q~n"),
%	{_Code, _Message, Body} = gen_server:call(?MODULE, {time, T1, T2}),
%	{ok, {array, Records}} = json2:decode_string(Body),
%	[io:format("~s, ~s, ~w, ~w~n", paper(Record)) || Record <- Records],
%	ok;
%
%select({time, T1, T2, Scale}) ->
%	io:format("q~n"),
%	{_Code, _Message, Body} = gen_server:call(?MODULE, {time, T1, T2, Scale}),
%	{ok, {array, Records}} = json2:decode_string(Body),
%	[io:format("~s, ~s, ~w, ~w, ~w, ~w, ~w~n", multifor(Record)) || Record <- Records],
%	ok;
%
%select({name, time, Name, T1, T2}) ->
%	io:format("q~n"),
%	{_Code, _Message, Body} = gen_server:call(?MODULE, {name, time, Name, T1, T2}),
%	{ok, {array, Records}} = json2:decode_string(Body),
%	[io:format("~s, ~s, ~w, ~w~n", paper(Record)) || Record <- Records],
%	ok;
%
%select({name, time, Name, T1, T2, Scale}) ->
%	io:format("q~n"),
%	{_Code, _Message, Body} = gen_server:call(?MODULE, {name, time, Name, T1, T2, Scale}),
%	{ok, {array, Records}} = json2:decode_string(Body),
%	[io:format("~s, ~s, ~w, ~w, ~w, ~w, ~w~n", multifor(Record)) || Record <- Records],
%	ok;

select(_) ->
	err.

add() ->
	Count = random:uniform(10000),
	[add_() || _ <- lists:seq(1, Count)],
	io:format("Amount of requests ~w~n", [Count]).

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

