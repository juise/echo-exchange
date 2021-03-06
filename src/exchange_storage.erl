-module(exchange_storage).
-author('Petrovsky Alexander, <askjuise@gmail.com>').

-include_lib("stdlib/include/ms_transform.hrl").

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).

-record(paper, {name, time, price, value}).
-record(multifor, {name, time, open_price, close_price, min_price, max_price, value}).


start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	self() ! init_storage,
	{ok, []}.

%% Scale record by minute
minute(#paper{name=N, time=T, price=P, value=V}) ->
	{D, {H, M, _}} = T,
	#paper{name=N, time={D, {H, M, 0}}, price=P, value=V}.

%% Scale record by hour
hour(#paper{name=N, time=T, price=P, value=V}) ->
	{D, {H, _, _}} = T,
	#paper{name=N, time={D, {H, 0, 0}}, price=P, value=V}.

%% Scale record by day
day(#paper{name=N, time=T, price=P, value=V}) ->
	{D, _} = T,
	#paper{name=N, time={D, {0, 0, 0}}, price=P, value=V}.

%% Scale record by week
week(#paper{name=N, time=T, price=P, value=V}) ->
	{{Y, M, D}, _} = T,
	{Y, W} = calendar:iso_week_number({Y, M, D}),
	#paper{name=N, time={{Y, M, W}, {0, 0, 0}}, price=P, value=V}.

%% Scale record by month
month(#paper{name=N, time=T, price=P, value=V}) ->
	{{Y, M, _}, _} = T,
	#paper{name=N, time={{Y, M, 1}, {0, 0, 0}}, price=P, value=V}.

scale([], _) ->
	[];

%% Scale records by minute
scale(Papers, minute) ->
	scale(lists:map(fun minute/1, Papers));

%% Scale records by hour
scale(Papers, hour) ->
	scale(lists:map(fun hour/1, Papers));

%% Scale records by day
scale(Papers, day) ->
	scale(lists:map(fun day/1, Papers));

%% Scale records by week
scale(Papers, week) ->
	scale(lists:map(fun week/1, Papers));

%% Scale records by month
scale(Papers, month) ->
	scale(lists:map(fun month/1, Papers)).





%% Scale records
scale([#paper{name=N, time=T, price=P, value=V} | Papers]) ->
	group(N, T, P, P, P, P, V, Papers, []).

%% Group records by Name and Scale
group(Name, Time, OpenPrice, _ClosePrice, MinPrice, MaxPrice, Value, [Paper = #paper{name=Name, time=Time} | Papers], Multifors) ->
	Min = min(MinPrice, Paper#paper.price),
	Max = max(MaxPrice, Paper#paper.price),
	group(Name, Time, OpenPrice, Paper#paper.price, Min, Max, Value+Paper#paper.value, Papers, Multifors);

group(Name, Time, OpenPrice, ClosePrice, MinPrice, MaxPrice, Value, [Paper | Papers], Multifors) when Name =/= Paper#paper.name orelse Time =/= Paper#paper.time ->
	Multifor = #multifor{name=Name, time=Time, open_price=OpenPrice, close_price=ClosePrice, min_price=MinPrice, max_price=MaxPrice, value=Value},
	group(Paper#paper.name, Paper#paper.time, Paper#paper.price, Paper#paper.price, Paper#paper.price, Paper#paper.price, Paper#paper.value, Papers, [Multifor | Multifors]);

group(Name, Time, OpenPrice, ClosePrice, MinPrice, MaxPrice, Value, [], Multifors) ->
	Multifor = #multifor{name=Name, time=Time, open_price=OpenPrice, close_price=ClosePrice, min_price=MinPrice, max_price=MaxPrice, value=Value},
	lists:sort([Multifor | Multifors]).


%% Select all
select_all() ->
	lists:sort(ets:select(paper, ets:fun2ms(fun(Paper) -> Paper end))).

%% Select all and Scale
select_all(Scale) ->
	Papers = select_all(),
	scale(Papers, Scale).

%% Select all by given Name
select_by_name(Name) ->
	lists:sort(ets:select(paper, ets:fun2ms(fun(Paper = #paper{name=N}) when N =:= Name -> Paper end))).

%% Select all by given Name and Scale
select_by_name(Name, Scale) ->
	Papers = select_by_name(Name),
	scale(Papers, Scale).

%% Select all by given date time T1,T2 range
select_by_time(T1, T2) ->
	lists:sort(ets:select(paper, ets:fun2ms(fun(Paper = #paper{time=T}) when T1 =< T andalso T =< T2 -> Paper end))).

%% Select all by given date time T1,T2 range and Scale
select_by_time(T1, T2, Scale) ->
	Papers = select_by_time(T1, T2),
	scale(Papers, Scale).

%% Select all by given Name, date time T1,T2 range
select_by_name_time(Name, T1, T2) ->
	lists:sort(ets:select(paper, ets:fun2ms(fun(Paper = #paper{name=N, time=T}) when N =:= Name andalso T1 =< T andalso T =< T2 -> Paper end))).

%% Select all by given Name, date time T1,T2 range and Scale
select_by_name_time(Name, T1, T2, Scale) ->
	Papers = select_by_name_time(Name, T1, T2),
	scale(Papers, Scale).


handle_call(all, _From, State) ->
	Reply = select_all(),
	{reply, Reply, State};

handle_call({all, Scale}, _From, State) ->
	Reply = select_all(Scale),
	{reply, Reply, State};

handle_call({name, Name}, _From, State) ->
	Reply = select_by_name(Name),
	{reply, Reply, State};

handle_call({name, Name, Scale}, _From, State) ->
	Reply = select_by_name(Name, Scale),
	{reply, Reply, State};

handle_call({time, Time1, Time2}, _From, State) ->
	Reply = select_by_time(Time1, Time2),
	{reply, Reply, State};

handle_call({time, Time1, Time2, Scale}, _From, State) ->
	Reply = select_by_time(Time1, Time2, Scale),
	{reply, Reply, State};

handle_call({name, time, Name, Time1, Time2}, _From, State) ->
	Reply = select_by_name_time(Name, Time1, Time2),
	{reply, Reply, State};

handle_call({name, time, Name, Time1, Time2, Scale}, _From, State) ->
	Reply = select_by_name_time(Name, Time1, Time2, Scale),
	{reply, Reply, State};

handle_call({add, Name, Time, Price, Value}, _From, State) ->
	true = ets:insert(paper, #paper{name=Name, time=Time, price=Price, value=Value}),
	{reply, ok, State}.


handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(init_storage, State) ->
	ets:new(paper, [bag, {keypos, #paper.name}, named_table]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

