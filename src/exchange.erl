-module(exchange).
-author('Petrovsky Alexander, <askjuise@gmail.com>').

-behaviour(application).

-export([start/0, start/2, stop/1]).


start() ->
	application:start(exchange).

start(normal, []) ->
	exchange_sup:start_link().

stop(_State) ->
	ok.

