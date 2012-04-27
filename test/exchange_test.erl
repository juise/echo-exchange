-module(exchange_test).
-author('Petrovsky Alexander, <askjuise@gmail.com>').

-include_lib("eunit/include/eunit.hrl").


start_test_() ->
	{"exchange_sup, exchange_server, exchange_storage, yaws webserver started test",
		{setup,
		fun start/0,
		fun is_registered/1
		}
	}.


start() ->
	{ok, Pid} = exchange_sup:start_link(),
	Pid.

is_registered(Pid) ->
	[?_assert(erlang:is_process_alive(Pid)),
	?_assertEqual(Pid, erlang:whereis(exchange_sup)),
	?_assertNotEqual(undefined, erlang:whereis(exchange_server)),
	?_assertNotEqual(undefined, erlang:whereis(exchange_storage)),
	?_assertNotEqual(undefined, erlang:whereis(yaws_server)),
	?_assertNotEqual(undefined, erlang:whereis(yaws_log))
	].

