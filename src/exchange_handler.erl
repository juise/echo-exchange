-module(exchange_handler).
-author('Petrovsky Alexander, <askjuise@gmail.com>').

-include("yaws.hrl").
-include("yaws_api.hrl").

-export([out/1, handle_request/3]).

-record(paper, {name, time, price, value}).
-record(multifor, {name, time, open_price, close_price, min_price, max_price, value}).

-define(CREATED, 201).
-define(BAD_REQUEST, 400).
-define(NOT_FOUND, 404).

-define(URL, "/api/v1/").

-define(is_scale(S), (S =:= "minute" orelse S =:= "hour" orelse S =:= "day" orelse S =:= "week" orelse S =:= "month")).
-define(is_time(T), (T =/= {error,bad_date})).


%% Transform record Paper to proplist
record_to_proplist([#paper{name=Name, time=Time, price=Price, value=Value} | Records], Props) ->
	Time1 = dh_date:format("Y-m-dTH:i:s", Time),
	Prop = {struct, [{name, Name}, {time, Time1}, {price, Price}, {value, Value}]},
	record_to_proplist(Records, [Prop | Props]);

%% Transform record Multifor to proplist
record_to_proplist([#multifor{name=Name, time=Time, open_price=OpenPrice, close_price=ClosePrice, min_price=MinPrice, max_price=MaxPrice, value=Value} | Records], Props) ->
	Time1 = dh_date:format("Y-m-dTH:i:s", Time),
	Prop = {struct, [{name, Name}, {time, Time1}, {open_price, OpenPrice}, {close_price, ClosePrice}, {min_price, MinPrice}, {max_price, MaxPrice}, {value, Value}]},
	record_to_proplist(Records, [Prop | Props]);

record_to_proplist([], Props) ->
	Props.

%% Generate JSON from records
json(Records) ->
	Proplist = record_to_proplist(Records, []),
	json2:encode({array, Proplist}).

dt(T) ->
	dh_date:parse(T).


out(Arg) ->
	Request = Arg#arg.req,
	Method = Request#http_request.method,
	{_, Path} = Request#http_request.path,
	handle_request(Method, Path, Arg).


handle_request(Method, ?URL++Path, Arg) ->
	handle_request(Method, string:tokens(Path, "/"), Arg);

%% Select all, url - /api/v1/all
handle_request('GET', ["all"], _Arg) ->
	Reply = gen_server:call(exchange_storage, all),
	handle_response(Reply);

%% Select all and Scale, url - /api/v1/all/Scale, Scale = minute | hour | day | week | month
handle_request('GET', ["all", Scale], _Arg) when ?is_scale(Scale) ->
	Reply = gen_server:call(exchange_storage, {all, list_to_atom(Scale)}),
	handle_response(Reply);

%% Select all by given Name, url - /api/v1/Name
handle_request('GET', [Name], _Arg) ->
	Reply = gen_server:call(exchange_storage, {name, Name}),
	handle_response(Reply);

%% Select all by given Name and Scale, url - /api/v1/Name/Scale, Scale = minute | hour | day | week | month
handle_request('GET', [Name, Scale], _Arg) when ?is_scale(Scale) ->
	Reply = gen_server:call(exchange_storage, {name, Name, list_to_atom(Scale)}),
	handle_response(Reply);

%% Select all by given date time T1,T2
handle_request('GET', [T1, T2], _Arg) ->
	handle_request_time([dt(T1), dt(T2)]);

%% Select all by given date time T1,T2 and Scale, url - /api/v1/Name/Scale, Scale = minute | hour | day | week | month
handle_request('GET', [T1, T2, Scale], _Arg) when ?is_scale(Scale) ->
	handle_request_time([dt(T1), dt(T2), Scale]);

%% Select all by given Name, date time T1,T2, url - /api/v1/T1/T2
handle_request('GET', [Name, T1, T2], _Arg) ->
	handle_request_name_time([Name, dt(T1), dt(T2)]);

%% Select all by given Name, date time T1,T2 and Scale, url - /api/v1/Name/Scale, Scale = minute | hour | day | week | month
handle_request('GET', [Name, T1, T2, Scale], _Arg) when ?is_scale(Scale) ->
	handle_request_name_time([Name, dt(T1), dt(T2), Scale]);

handle_request('POST', ["add"], Arg) ->
	try
		{ok, Name} = yaws_api:postvar(Arg, "name"),
		{ok, Time} = yaws_api:postvar(Arg, "time"),
		{ok, Price} = yaws_api:postvar(Arg, "price"),
		{ok, Value} = yaws_api:postvar(Arg, "value"),

		N = string:to_upper(string:sub_string(Name, 1, 4)),
		P = list_to_integer(Price),
		V = list_to_integer(Value),

		handle_request_post(N, dt(Time), P, V)
	catch
		_ -> handle_response(?BAD_REQUEST)
	end;

handle_request(_Method, _Request, _Arg) ->
	{status, ?NOT_FOUND}.


handle_request_time([T1, T2]) when ?is_time(T1), ?is_time(T2) ->
	Reply = gen_server:call(exchange_storage, {time, T1, T2}),
	handle_response(Reply);

handle_request_time([T1, T2, Scale]) when ?is_time(T1), ?is_time(T2) ->
	Reply = gen_server:call(exchange_storage, {time, T1, T2, list_to_atom(Scale)}),
	handle_response(Reply);

handle_request_time(_) ->
	handle_response(?BAD_REQUEST).

handle_request_name_time([Name, T1, T2]) when ?is_time(T1), ?is_time(T2) ->
	Reply = gen_server:call(exchange_storage, {name, time, Name, T1, T2}),
	handle_response(Reply);

handle_request_name_time([Name, T1, T2, Scale]) when ?is_time(T1), ?is_time(T2) ->
	Reply = gen_server:call(exchange_storage, {name, time, Name, T1, T2, list_to_atom(Scale)}),
	handle_response(Reply);

handle_request_name_time(_) ->
	handle_response(?BAD_REQUEST).

handle_request_post(Name, Time, Price, Value) when ?is_time(Time) ->
	ok = gen_server:call(exchange_storage, {add, Name, Time, Price, Value}),
	handle_response(?CREATED);

handle_request_post(_Name, _Time, _Price, _Value) ->
	handle_response(?BAD_REQUEST).


handle_response(?CREATED) ->
	{status, ?CREATED};

handle_response(?BAD_REQUEST) ->
	{status, ?BAD_REQUEST};

handle_response([]) ->
	{status, ?NOT_FOUND};

handle_response(Reply) ->
	[{status, 200}, {header, {"Vary", "Accept"}}, {content, "application/json; charset=utf-8", json(Reply)}].

