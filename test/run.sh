#!/bin/bash
erl -compile $1
erl -noshell -s $1 start -name $1
