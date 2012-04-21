#!/bin/sh

erl -make
erl -noshell -pa ./priv/yaws/ebin/ -pa ./ebin/ -s exchange start

