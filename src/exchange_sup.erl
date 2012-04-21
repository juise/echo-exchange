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
	io:format("Try to start exchange_server.~n"),
	Exchange = {exchange_server, {exchange_server, start_link, []}, permanent, 1000, worker, [exchange_server]},
	{ok, {
			{RestartStrategy, MaxRestart, MaxTime},
			[Exchange]
		}
	}.

