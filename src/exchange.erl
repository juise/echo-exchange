-module(exchange).

-include("exchange.hrl").

-behaviour(gen_server).

-export([start_link/0, add_paper/5, list_papers/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).


start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	io:format("exchange started~n"),
	{ok, []}.

add_paper(Pid, Name, Time, Price, Value) ->
	gen_server:call(Pid, {add, #paper{name=Name, time=Time, price=Price, value=Value}}).

del_paper(Pid, Name, Time, Price, Value) ->
	gen_server:call(Pid, {del, #paper{name=Name, time=Time, price=Price, value=Value}}).

handle_cast(list, Papers) ->
	{noreply, Papers}.

handle_call({add, Paper = #paper{}}, _From, Papers) ->
	{reply, Paper, [Paper | Papers]}.

handle_call({del, Paper = #paper{}}, _From, Papers) ->
	{reply, Paper, [Paper | Papers]}.

handle_info(_Info, Papers) ->
	{noreply, Papers}.

terminate(_Reason, Papers) ->
	ok.

code_change(_OldVsn, Papers, _Extra) ->
	{ok, Papers}.
