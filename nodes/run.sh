#!/bin/bash
erl -compile bootstrap
erl -noshell -s bootstrap start
