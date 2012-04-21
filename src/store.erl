-module(store).

-include_lib("stdlib/include/ms_transform.hrl").

-export([init/0, add/0, add/4, select_all/0, select_all/1, select_by_price/2, select_by_time/3, select_by_value/2, select_by_value/3]).

-record(paper, {name, time, price, value}).
-record(multifor, {name, time, open_price, close_price, min_price, max_price, value}).


init() ->
	Papers = [
			#paper{name="YNDX", time=calendar:local_time(), price=10, value=20},
			#paper{name="GOOG", time={{2012,3,20},{15,0,0}}, price=20, value=30},
			#paper{name="YNDX", time={{2012,4,20},{15,30,59}}, price=16, value=510},
			#paper{name="GOOG", time=calendar:local_time(), price=200, value=540},
			#paper{name="YNDX", time=calendar:local_time(), price=50, value=20},
			#paper{name="YNDX", time=calendar:local_time(), price=20, value=400},
			#paper{name="YNDX", time={{2012,4,20},{12,9,59}}, price=40, value=10},
			#paper{name="GOOG", time=calendar:local_time(), price=60, value=50},
			#paper{name="GOOG", time={{2012,4,21},{12,9,59}}, price=340, value=500}],
	ets:new(paper, [bag, {keypos, #paper.name}, named_table]),
	ets:insert(paper, Papers).


minute(#paper{name=N, time=T, price=P, value=V}) ->
	{D, {H, M, _}} = T,
	#paper{name=N, time={D, {H, M, 0}}, price=P, value=V}.

hour(#paper{name=N, time=T, price=P, value=V}) ->
	{D, {H, _, _}} = T,
	#paper{name=N, time={D, {H, 0, 0}}, price=P, value=V}.

day(#paper{name=N, time=T, price=P, value=V}) ->
	{D, _} = T,
	#paper{name=N, time={D, {0, 0, 0}}, price=P, value=V}.

week(#paper{name=N, time=T, price=P, value=V}) ->
	{{Y, M, D}, _} = T,
	{Y, W} = calendar:iso_week_number({Y, M, D}),
	#paper{name=N, time={{Y, M, W}, {0, 0, 0}}, price=P, value=V}.

month(#paper{name=N, time=T, price=P, value=V}) ->
	{{Y, M, _}, _} = T,
	#paper{name=N, time={{Y, M, 1}, {0, 0, 0}}, price=P, value=V}.

%get_week_number({Y, M, D}) ->
%	M1 = M - 1,
%	D1 = last_day_of_the_month({Y, M1}),
%	{_, W1} = calendar:iso_week_number({Y, M, D}),
%	{_, W2} = calendar:iso_week_number({Y, M1, D1}),

%	if W=W2-W1 =< 5 ->
%		W;
%	true -> 1
%	end


scale(Papers, minute) ->
	scale(lists:map(fun minute/1, Papers));

scale(Papers, hour) ->
	scale(lists:map(fun hour/1, Papers));

scale(Papers, day) ->
	scale(lists:map(fun day/1, Papers));

scale(Papers, week) ->
	scale(lists:map(fun week/1, Papers));

scale(Papers, month) ->
	scale(lists:map(fun month/1, Papers)).

scale([#paper{name=N, time=T, price=P, value=V} | Papers]) ->
	group(N, T, P, P, P, P, V, Papers, []).

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


add() ->
	Paper = [#paper{name="ECHO", time=calendar:local_time(), price=10, value=20}],
	ets:insert(paper, Paper).

add(Name, Time, Price, Value) ->
	Paper = #paper{name=Name, time=Time, price=Price, value=Value},
	ets:insert(paper, Paper).

select_all() ->
	ets:select(paper, ets:fun2ms(fun(Paper) -> Paper end)).

select_all(Scale) ->
	Papers = ets:select(paper, ets:fun2ms(fun(Paper) -> Paper end)),
	scale(Papers, Scale).

select_by_price(Price, Scale) ->
	Papers = ets:select(paper, ets:fun2ms(fun(Paper = #paper{price=P}) when P =:= Price -> Paper end)),
	scale(Papers, Scale).

select_by_time(T1, T2, Scale) ->
	Papers = ets:select(paper, ets:fun2ms(fun(Paper = #paper{time=T}) when T1 =< T andalso T =< T2 -> Paper end)),
	scale(Papers, Scale).

select_by_value(Value, Scale) ->
	Papers = ets:select(paper, ets:fun2ms(fun(Paper = #paper{value=V}) when V =:= Value -> Paper end)),
	scale(Papers, Scale).

select_by_value(Value, ">", Scale) ->
	Papers = ets:select(paper, ets:fun2ms(fun(Paper = #paper{value=V}) when V > Value -> Paper end)),
	scale(Papers, Scale);

select_by_value(Value, "<", Scale) ->
	Papers = ets:select(paper, ets:fun2ms(fun(Paper = #paper{value=V}) when V < Value -> Paper end)),
	scale(Papers, Scale);

select_by_value(Value, "=<", Scale) ->
	Papers = ets:select(paper, ets:fun2ms(fun(Paper = #paper{value=V}) when V =< Value -> Paper end)),
	scale(Papers, Scale);

select_by_value(Value, ">=", Scale) ->
	Papers = ets:select(paper, ets:fun2ms(fun(Paper = #paper{value=V}) when V >= Value -> Paper end)),
	scale(Papers, Scale).

