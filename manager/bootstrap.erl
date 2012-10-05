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
-include("../shared/config.hrl").

%%
%% Will check the healty of each node. Each node should to send a
%% message with the atom "active" to the manager, in case of don't
%% receive during ?TIME_TO_REFRESH_NODE_MS miliseconds, remove the
%% node from the nodes lists, should to be run on a new process
%%
%% @param NodePid pid The pid of the node to check
%%
check_node_status(NodePid) ->
    receive
        % Only to know if the node is still alive
        active ->
            io:format("Refresh active node~n"),
            check_node_status(NodePid)

    % If we don't receive a communication from the node in the
    % lasts TIME_TO_REFRESH_NODE_MS the node is down, remove it
    after ?TIME_TO_REFRESH_NODE_MS ->
        io:format(standard_error, "Time out on node ~w~n", [NodePid]),
        manager ! {remove_node, NodePid}
    end.

%%
%% Sends the current list of active nodes to all the active nodes
%% in a recursive iteration over the list of active nodes
%%
%% @param [Node_to_update | Nodes_not_updated] list List of nodes to update
%% @param ActiveNodes list List of active nodes
%%
update_nodes_lists([Node_to_update | Nodes_not_updated], ActiveNodes) ->
    Node_to_update ! {update_nodes_list, ActiveNodes},

    if
        length(Nodes_not_updated) > 0 ->
            update_nodes_lists(Nodes_not_updated, ActiveNodes)
    end.

%%
%% Listen for the messages to add or remove nodes from the nodes
%% list, or get the current nodes list
%%
start_listener(Is_master, Nodes) ->
    io:format("Current Nodes ~w~n", [Nodes]),

    receive
        % Adds a node with pid "Pid" to the list of nodes and starts
        % monitorization
        {add_node, Pid} ->
            % Check if the pid is in the list to don't add it twice
            Yet_in_list = lists:member(Pid, Nodes),

            if
                not Yet_in_list ->
                    io:format("Adding node ~w~n", [string:concat("check_", pid_to_list(Pid))]),
                    % Add the supervisor loop, controls that the node is active,
                    % and in case of a down, remove it
                    register(
                        list_to_atom(string:concat("check_", pid_to_list(Pid))),
                        spawn(bootstrap, check_node_status, [Pid])),

                    % Update the list of nodes on each active node
                    update_nodes_lists(Nodes, Nodes),
                    start_listener(Is_master, [Pid | Nodes]);
                true ->
                    io:format("Node yet added ~w~n", [Nodes]),
                    start_listener(Is_master, Nodes)
            end;

        % Remove a node with pid "Pid" from the list of nodes
        {remove_node, Pid} ->
            io:format("Removing node node ~w~n", [Pid]),
            Active_nodes = lists:delete(Pid, Nodes),
            % Update the list of nodes on each active node
            update_nodes_lists(Active_nodes, Active_nodes),
            start_listener(Is_master, Active_nodes);

        % Sends the list of active nodes to the node
        {get_nodes, Pid} ->
            Pid ! Nodes,
            start_listener(Is_master, Nodes)
    end.

%%
%% Main process, get the parameters from the terminal, and after check
%% if all is correct, launch the messages listener
%%
start() ->
    case init:get_argument(node_type) of
        % node_name param present, check if the node is the master or the slave
        {ok, [["master"]]} ->
            Node_addr = ?MASTER_MANAGER,
            Is_master = true;

        {ok, [["slave"]]} ->
            Node_addr = ?SLAVE_MANAGER,
            Is_master = false;

        % Node type not specified
        error ->
            Node_addr = '',
            Is_master = false,
            io:format("Error:~nExecute command: erl -noshell -s bootstrap start -node_type [master|slave]~n", []),
            exit(node_name_not_found)
    end,

    % Check if the address specified by the user on the config file
    % is the same than the real node address
    if
        node() == Node_addr ->
            io:format("Node Name: \"~w\" @ \"~w\"~nStarting listener...~n", [manager, node()]),
            register(manager, self()),
            start_listener(Is_master, []);

        true ->
            io:format("The configured node address: \"~w\" doesn't correspond with the current address: \"~w\"~n", [Node_addr, node()]),
            exit(manager_not_allowed)
    end.
