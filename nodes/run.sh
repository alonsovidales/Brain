#!/bin/bash
erl -compile bootstrap
erl -noshell -s bootstrap start -name node$1
