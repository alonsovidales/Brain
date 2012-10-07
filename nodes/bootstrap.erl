%%
%% Run this file on each master node
%%
%% @author Alonso Vidales <alonso.vidales@tras2.es>
%% @version 0.1
%% @since 2012-10-02
%% @see config.hrl
%%

-module(bootstrap).
-export([start/0, listener_loop/3]).
-include("../shared/config.hrl").

%%
%% Used to send a signal each TIME_TO_REFRESH_NODE_MS / 2 miliseconds
%% to the master node in order to know which nodes are alive
%%
%% @param Listener_uniq_idlist  A list of chars with an uniq
%%                              identifier for this node
%%
keep_alive(Listener_uniq_id) ->
    timer:sleep(round(?TIME_TO_REFRESH_NODE_MS / 2)),
    io:format("Sending alive signal ~w~n", [list_to_atom(Listener_uniq_id)]),
    {list_to_atom(Listener_uniq_id), ?MASTER_MANAGER} ! im_alive,
    keep_alive(Listener_uniq_id).

get_node_pos([{Node_name, Pid} | Rest], Current_pos) ->
    if
        Node_name == node() ->
            Current_pos;
        true ->
            get_node_pos(Rest, Current_pos + 1)
    end.

ask_for_object_to_neighbour(Object_id, Neightbour) ->
    Neightbour ! {s, true, Object_id, self()},
    receive
        {ok, Value} ->
            Value;
        ko ->
            ko
    end.

listener_loop(Current_nodes, Objects_dict, Neighbours) ->
    io:format("Listener loop, Current_nodes: ~w~n", [Current_nodes]),
    receive
        % Save a new object on the local memory dictionary
        {s, Object_id, Value} ->
            io:format("Save Object Key: \"~w\" Value: \"~w\"~n", [Object_id, Value]),
            listener_loop(Current_nodes, dict:append(Object_id, Value, Objects_dict), Neighbours);

        % Get an object from memory with Object_id as key, if Consistency is true,
        % the system will askto all the nodes until find it.
        {g, Consistency, Object_id, Pid} ->
            io:format("Looking for global object ~w~n", [Object_id]),
            Value = dict:find(Object_id),
            case Value of
                {ok, Value} ->
                    io:format("Value found for key ~w: \"~w\"~n", [Object_id, Value]);
                    Pid ! {ok, Value},
                error ->
                    if
                        Consistency ->
                            io:format("Key ~w not found, seach on partners~n", [Object_id]),
                            [Neightbour_left, Neightbour_right] = Neighbours,
                            case ask_for_object_to_neighbour(Object_id, Neightbour_left) of
                                ko ->
                                    case ask_for_object_to_neighbour(Object_id, Neightbour_right) of
                                        ko ->
                                            Pid ! ko;
                                        Value ->
                                            Pid ! {ok, Value}
                                    end;
                                Value ->
                                    Pid ! {ok, Value}
                            end;
                        true ->
                            io:format("Key ~w not found, don't use consistency~n", [Object_id])
                            Pid ! ko,
                    end
            end,

            listener_loop(Current_nodes, Objects_dict, Neighbours);

        % Updates the list of active nodes
        {update_nodes_list, Nodes} ->
            io:format("Nodes list updated: ~w~n", [Nodes]),
            % Check the number of active nodes in order to define the deterinate the
            % two adjacent nodes to be asked when the client nedds a shared object
            if
                % You are the only one, forever alone :'(
                length(Nodes) == 1 ->
                    New_neighbours = [];
                % Only a neightbour, use it
                length(Nodes) == 2 ->
                    case get_node_pos(Nodes, 1) of
                        1 ->
                            New_neighbours = [lists:nth(2, Nodes)];
                        2 ->
                            New_neighbours = [lists:nth(1, Nodes)]
                    end;
                % Two or more neightbours, get the adjacents
                true ->
                    Pos = get_node_pos(Nodes, 1),
                    if
                        Pos == 1 ->
                            New_neighbours = [lists:nth(2, Nodes), lists:last(Nodes)];
                        Pos == length(Nodes) ->
                            New_neighbours = [lists:nth(1, Nodes), lists:nth(Pos - 1, Nodes)];
                        true ->
                            New_neighbours = [lists:nth(Pos + 1, Nodes), lists:nth(Pos - 1, Nodes)]
                    end
            end,
            io:format("New neighbours: ~w~n", [New_neighbours]),
            listener_loop(Nodes, Objects_dict, New_neighbours)
    end.
    

%%
%% Main process, get the parameters from the terminal, and after check
%% if all is correct, launch the messages listener
%%
start() ->
    Listener_pid = spawn(bootstrap, listener_loop, [[], dict:new(), []]),

    % Using the name@domain to create a global uniqe identifier, we can't
    % have more than a node on the same machine with the same name
    Check_uniq_id = atom_to_list(node()),
    io:format("UID ~w~n", [list_to_atom(Check_uniq_id)]),
    {manager, ?MASTER_MANAGER} ! {add_node, Listener_pid, Check_uniq_id},
    keep_alive(Check_uniq_id),

    exit(normal).
