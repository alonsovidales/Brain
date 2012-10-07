#!/bin/bash
erl -compile add_object_test
erl -noshell -s add_object_test start -name add_object_test
