{application, exchange,
 [{description, "Exchange application"},
  {vsn, "0.1"},
  {modules, [exchange_server, exchange_sup]},
  {registered, []},
  {env, []},
  {applications, [kernel, stdlib]},
  {mod, {exchange, []}}
 ]}.

