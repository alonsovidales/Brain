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
-include("includes/config.hrl").

start_listener(Nodes) ->
    io:format("Current Nodes ~w~n", [Nodes]),

    receive
        {add_node, Pid} ->
            Yet_in_list = lists:member(Pid, Nodes),

            if
                not Yet_in_list ->
                    io:format("Adding node ~w~n", [Pid | Nodes]),
                    start_listener([Pid | Nodes]);
                true ->
                    io:format("Node yet added ~w~n", [Nodes]),
                    start_listener(Nodes)
            end;
        {remove_node, Pid} ->
            io:format("Removing node node ~w~n", [Pid]),
            start_listener(lists:delete(Pid, Nodes))
    end.

start() ->
    io:format("Starting listener~n"),
    register(manager, self()),
    start_listener([]).
