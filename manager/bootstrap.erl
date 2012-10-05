%%
%% Run this file on each master node
%%
%% @author Alonso Vidales <alonso.vidales@tras2.es>
%% @version 0.1
%% @since 2012-10-02
%% @see config.hrl
%%

-module(bootstrap).
-export([start/0, check_node_status/1]).
-include("includes/config.hrl").

check_node_status(NodePid) ->
    receive
        active ->
            io:format("Refresh active node~n"),
            check_node_status(NodePid)

    after ?TIME_TO_REFRESH_NODE_MS ->
        io:format("Time out on node ~w~n", [NodePid]),
        manager ! {remove_node, NodePid}
    end.

start_listener(Nodes) ->
    io:format("Current Nodes ~w~n", [Nodes]),

    receive
        {add_node, Pid} ->
            % Check if the pid is in the list to don't add it twice
            Yet_in_list = lists:member(Pid, Nodes),

            if
                not Yet_in_list ->
                    io:format("Adding node ~w~n", [string:concat("check_", pid_to_list(Pid))]),
                    register(list_to_atom(string:concat("check_", pid_to_list(Pid))), spawn(bootstrap, check_node_status, [Pid])),
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
