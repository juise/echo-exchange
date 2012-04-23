#!/bin/sh

erl -make
erl -pa ./priv/yaws/ebin/ -pa ./priv/erlang_util/ebin/ -pa ./ebin/ -s exchange start
#erl -noshell -pa ./priv/yaws/ebin/ -pa ./ebin/ -s exchange start

