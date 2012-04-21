-module(exchange_server).
-author('Petrovsky Alexander, <askjuise@gmail.com>').

-include_lib("yaws.hrl").

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).


start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	self() ! init_yaws,
	{ok, []}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_call(_Msg, _From, State) ->
	{reply, State}.

handle_info(init_yaws, State) ->
	Id = "embedded",
	Docroot = "/tmp",
	GconfList = [{id, Id}, {logdir, "/tmp"}],
	SconfList = [{port, 8888}, {servername, "localhost"}, {listen, {0,0,0,0}}, {docroot, Docroot}],
				%{appmods, [{"/", exchange_handler}]}
				%],
	{ok, SCList, GC, ChildSpecs} = yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
	[supervisor:start_child(exchange_sup, Ch) || Ch <- ChildSpecs],
	yaws_api:setconf(GC, SCList),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

