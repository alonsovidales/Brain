#!/bin/bash
erl -compile bootstrap
erl -noshell -s bootstrap start -name manager -node_type master
