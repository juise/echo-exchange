{application, exchange,
 [{description, "Exchange application"},
  {vsn, "0.1"},
  {modules, [exchange_handler, exchange_server, exchange_storage, exchange_sup]},
  {registered, []},
  {env, []},
  {applications, [kernel, stdlib]},
  {mod, {exchange, []}}
 ]}.

