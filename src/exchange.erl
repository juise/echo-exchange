-module(exchange).
-author('Petrovsky Alexander, <askjuise@gmail.com>').

-behaviour(application).

-export([start/2, stop/1]).


start(_Type, _Args) ->
	exchange_sup:start_link().

stop(_State) ->
	ok.

