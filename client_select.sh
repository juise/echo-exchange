#!/bin/sh

erl -make
erl -noshell -pa ./priv/yaws/ebin/ -pa ./priv/erlang_util/ebin/ -pa ./ebin/ -s exchange_client select $1

