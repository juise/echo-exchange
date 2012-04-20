-module(store).

-include_lib("stdlib/include/ms_transform.hrl").

-export([init/0, add/0, scale/1, select_all/0, select_by_price/1, select_by_time/2, select_by_value/1, select_by_value/2]).

-record(paper, {name, time, price, value}).
-record(paperf, {name, time, open_price, close_price, min_price, max_price, value}).


init() ->
	Papers = [#paper{name="ECHO", time=calendar:local_time(), price=10, value=20},
			#paper{name="ECHO", time=calendar:local_time(), price=20, value=30},
			#paper{name="YNDX", time=calendar:local_time(), price=16, value=510},
			#paper{name="GOOG", time=calendar:local_time(), price=200, value=540},
			#paper{name="ECHO", time=calendar:local_time(), price=50, value=20},
			#paper{name="YNDX", time=calendar:local_time(), price=20, value=400},
			#paper{name="YNDX", time=calendar:local_time(), price=40, value=10},
			#paper{name="GOOG", time=calendar:local_time(), price=60, value=50},
			#paper{name="ECHO", time=calendar:local_time(), price=340, value=500}],
	ets:new(paper, [bag, {keypos, #paper.name}, named_table]),
	ets:insert(paper, Papers).

add() ->
	Papers = [#paper{name="ECHO", time=calendar:local_time(), price=10, value=20}],
	ets:insert(paper, Papers).

scale(Papers) ->
	CPapers = lists:map(fun(#paper{name=N, time=T, price=P, value=V}) -> {D, {H, _, _}} = T, #paper{name=N, time={D, {H, 0, 0}}, price=P, value=V} end, Papers),
	scale_(CPapers).

%% Universeal functions
scale_([#paper{name=N, time=T, price=P, value=V} | Papers]) ->
	scale_(N, T, P, P, P, P, V, Papers, []).


scale_(Name, Time, OP, _CP, MINP, MAXP, V, [Paper = #paper{name=Name, time=Time} | Papers], Papersf) ->
	Min = min(MINP, Paper#paper.price),
	Max = max(MAXP, Paper#paper.price),

	scale_(Name, Time, OP, Paper#paper.price, Min, Max, V+Paper#paper.value, Papers, Papersf);


scale_(Name, Time, OP, CP, MINP, MAXP, V, [Paper | Papers], Papersf) when Name =/= Paper#paper.name orelse Time =/= Paper#paper.time ->
%%	if Name =/= Paper#paper.name; Time =/= Paper#paper.time ->
		Paperf = #paperf{name=Name, time=Time, open_price=OP, close_price=CP, min_price=MINP, max_price=MAXP, value=V},
		scale_(Paper#paper.name, Paper#paper.time, Paper#paper.price, Paper#paper.price, Paper#paper.price, Paper#paper.price, Paper#paper.value, Papers, [Paperf | Papersf]);
%%	true -> false
%%		if MINP > Paper#paper.price -> Min = Paper#paper.price;
%%		true -> Min = MINP
%%		end,
%%
%%		if MAXP < Paper#paper.price -> Max = Paper#paper.price;
%%		true -> Max = MINP
%%		end,
%%		scale_(Name, Time, OP, Paper#paper.price, Min, Max, V+Paper#paper.value, Papers, Papersf)
%%	end;

scale_(Name, Time, OP, CP, MINP, MAXP, V, [], Papersf) ->
	Paperf = #paperf{name=Name, time=Time, open_price=OP, close_price=CP, min_price=MINP, max_price=MAXP, value=V},
	[Paperf | Papersf].



select_all() ->
	Papers = ets:select(paper, ets:fun2ms(fun(Paper) -> Paper end)),
	scale(Papers).

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
