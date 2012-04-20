-module(store).

-include_lib("stdlib/include/ms_transform.hrl").

-export([init/0, add/0, scale/2, select_all/1, select_by_price/1, select_by_time/2, select_by_value/1, select_by_value/2]).

-record(paper, {name, time, price, value}).
-record(paperf, {name, time, open_price, close_price, min_price, max_price, value}).


init() ->
	Papers = [#paper{name="YNDX", time=calendar:local_time(), price=10, value=20},
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

add() ->
	Papers = [#paper{name="ECHO", time=calendar:local_time(), price=10, value=20}],
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

%get_week_number({Y, M, D}) ->
%	M1 = M - 1,
%	D1 = last_day_of_the_month({Y, M1}),
%	{_, W1} = calendar:iso_week_number({Y, M, D}),
%	{_, W2} = calendar:iso_week_number({Y, M1, D1}),

%	if W=W2-W1 =< 5 ->
%		W;
%	true -> 1
%	end

month(#paper{name=N, time=T, price=P, value=V}) ->
	{{Y, M, _}, _} = T,
	#paper{name=N, time={{Y, M, 1}, {0, 0, 0}}, price=P, value=V}.

scale(Papers, minute) ->
	%%CPapers = lists:map(fun(#paper{name=N, time=T, price=P, value=V}) -> {D, {H, M, _}} = T, #paper{name=N, time={D, {H, M, 0}}, price=P, value=V} end, Papers),
	CPapers = lists:map(fun minute/1, Papers),
	scale_(CPapers);

scale(Papers, hour) ->
%%	CPapers = lists:map(fun(#paper{name=N, time=T, price=P, value=V}) -> {D, {H, _, _}} = T, #paper{name=N, time={D, {H, 0, 0}}, price=P, value=V} end, Papers),
	CPapers = lists:map(fun hour/1, Papers),
	scale_(CPapers);

scale(Papers, day) ->
%%	CPapers = lists:map(fun(#paper{name=N, time=T, price=P, value=V}) -> {D, _} = T, #paper{name=N, time={D, {0, 0, 0}}, price=P, value=V} end, Papers),
	CPapers = lists:map(fun day/1, Papers),
	scale_(CPapers);

scale(Papers, week) ->
%	CPapers = lists:map(fun(#paper{name=N, time=T, price=P, value=V}) ->
%			{{Y, M, D}, _} = T,
%			{Y, W} = calendar:iso_week_number({Y, M, D}),
%			#paper{name=N, time={{Y, M, W}, {0, 0, 0}}, price=P, value=V} end, Papers),
	CPapers = lists:map(fun week/1, Papers),
	scale_(CPapers);

scale(Papers, month) ->
%	 CPapers = lists:map(fun(#paper{name=N, time=T, price=P, value=V}) -> {{Y, M, _D}, _} = T, #paper{name=N, time={{Y, M, 1}, {0, 0, 0}}, price=P, value=V} end, Papers),
	CPapers = lists:map(fun month/1, Papers),
	scale_(CPapers).


%% Universeal functions
scale_([#paper{name=N, time=T, price=P, value=V} | Papers]) ->
	scale_(N, T, P, P, P, P, V, Papers, []).

%% Accumulate list
scale_(Name, Time, OP, _CP, MINP, MAXP, V, [Paper = #paper{name=Name, time=Time} | Papers], Papersf) ->
	Min = min(MINP, Paper#paper.price),
	Max = max(MAXP, Paper#paper.price),
	scale_(Name, Time, OP, Paper#paper.price, Min, Max, V+Paper#paper.value, Papers, Papersf);

%% Write old list, start accumulate new list
scale_(Name, Time, OP, CP, MINP, MAXP, V, [Paper | Papers], Papersf) when Name =/= Paper#paper.name orelse Time =/= Paper#paper.time ->
	Paperf = #paperf{name=Name, time=Time, open_price=OP, close_price=CP, min_price=MINP, max_price=MAXP, value=V},
	scale_(Paper#paper.name, Paper#paper.time, Paper#paper.price, Paper#paper.price, Paper#paper.price, Paper#paper.price, Paper#paper.value, Papers, [Paperf | Papersf]);

scale_(Name, Time, OP, CP, MINP, MAXP, V, [], Papersf) ->
	Paperf = #paperf{name=Name, time=Time, open_price=OP, close_price=CP, min_price=MINP, max_price=MAXP, value=V},
	lists:sort([Paperf | Papersf]).



select_all(Scale) ->
	Papers = ets:select(paper, ets:fun2ms(fun(Paper) -> Paper end)),
	scale(Papers, Scale).

select_by_price(Price) ->
	Papers = ets:select(paper, ets:fun2ms(fun(Paper = #paper{price=P}) when P =:= Price -> Paper end)),
	Papers.

select_by_time(T1, T2) ->
	Papers = ets:select(paper, ets:fun2ms(fun(Paper = #paper{time=T}) when T1 =< T andalso T =< T2 -> Paper end)),
	Papers.

select_by_value(Value) ->
	Papers = ets:select(paper, ets:fun2ms(fun(Paper = #paper{value=V}) when V =:= Value -> Paper end)),
	Papers.

select_by_value(Value, ">") ->
	Papers = ets:select(paper, ets:fun2ms(fun(Paper = #paper{value=V}) when V > Value -> Paper end)),
	Papers;

select_by_value(Value, "<") ->
	Papers = ets:select(paper, ets:fun2ms(fun(Paper = #paper{value=V}) when V < Value -> Paper end)),
	Papers;

select_by_value(Value, "=<") ->
	Papers = ets:select(paper, ets:fun2ms(fun(Paper = #paper{value=V}) when V =< Value -> Paper end)),
	Papers;

select_by_value(Value, ">=") ->
	Papers = ets:select(paper, ets:fun2ms(fun(Paper = #paper{value=V}) when V >= Value -> Paper end)),
	Papers.
