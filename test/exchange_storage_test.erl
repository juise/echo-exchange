-module(exchange_storage_test).
-author('Petrovsky Alexander, <askjuise@gmail.com>').

-include_lib("eunit/include/eunit.hrl").


start_test_() ->
	{"exchange_storage test",
		{setup,
		fun start/0,
		fun select/1
		}
	}.


start() ->
	{ok, Pid} = exchange_storage:start_link(),
	Pid.

select(Pid) ->
	gen_server:call(exchange_storage, all)

	[?_assert(erlang:is_process_alive(Pid)),
	?_assertEqual(Pid, erlang:whereis(exchange_sup)),
	?_assertNotEqual(undefined, erlang:whereis(exchange_server)),
	?_assertNotEqual(undefined, erlang:whereis(exchange_storage)),
	?_assertNotEqual(undefined, erlang:whereis(yaws_server)),
	?_assertNotEqual(undefined, erlang:whereis(yaws_log))
	].

