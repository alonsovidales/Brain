%%
%% Run this file on each master node
%%
%% @author Alonso Vidales <alonso.vidales@tras2.es>
%% @version 0.1
%% @since 2012-10-02
%% @see config.hrl
%%

-module(bootstrap).
-export([start/0]).
-include("../shared/config.hrl").

%%
%% Main process, get the parameters from the terminal, and after check
%% if all is correct, launch the messages listener
%%
start() ->
    exit(normal).
