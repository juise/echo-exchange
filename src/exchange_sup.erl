-module(exchange_sup).
-author('Petrovsky Alexander, <askjuise@gmail.com>').

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	init({one_for_one, 60, 120});

init({RestartStrategy, MaxRestart, MaxTime}) ->
	ExchangeServer = {exchange_server, {exchange_server, start_link, []}, permanent, 1000, worker, [exchange_server]},
	ExchangeStrorage = {exchange_storage, {exchange_storage, start_link, []}, permanent, 1000, worker, [exchange_storage]},
	{ok, {
			{RestartStrategy, MaxRestart, MaxTime},
			[ExchangeServer, ExchangeStrorage]
		}
	}.

