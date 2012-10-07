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

get_node_pos([{Node_name, _Pid} | Rest], Current_pos) ->
    if
        Node_name == node() ->
            Current_pos;
        true ->
            get_node_pos(Rest, Current_pos + 1)
    end.

listener_loop(Current_nodes, Objects_dict, Neighbour) ->
    io:format("Listener loop, Current_nodes: ~w~n", [Current_nodes]),
    receive
        % Save a new object on the local memory dictionary
        {s, Object_id, Value} ->
            io:format("Save Object Key: \"~w\" Value: \"~w\"~n", [list_to_atom(Object_id), list_to_atom(Value)]),
            listener_loop(Current_nodes, dict:store(Object_id, Value, Objects_dict), Neighbour);

        % Get an object from memory with Object_id as key, if Consistency is true,
        % the system will askto all the nodes until find it.
        {g, Consistency, Object_id, Pid, Origin_pid} ->
            io:format("Looking for global object ~w~n", [list_to_atom(Object_id)]),
            case dict:find(Object_id, Objects_dict) of
                {ok, Value} ->
                    io:format("Value found for key ~w: \"~w\"~n", [list_to_atom(Object_id), list_to_atom(Value)]),
                    Pid ! {ok, Value};
                error ->
                    {Neighbour_id, Neighbour_pid} = Neighbour,
                    io:format("Asking to Neighbour \"~w\" from \"~w\"~n", [Neighbour_id, node()]),
                    if
                        Origin_pid == null ->
                            Origin_node = node();
                        true ->
                            Origin_node = Origin_pid
                    end,

                    if
                        Consistency and (Pid /= self()) and (Neighbour_id /= Origin_node) ->
                            io:format("Key ~w not found, seaching on neightbour~n", [list_to_atom(Object_id)]),
                            Neighbour_pid ! {g, Consistency, Object_id, Pid, Origin_node};
                        true ->
                            io:format("Key ~w not found, don't use consistency~n", [list_to_atom(Object_id)]),
                            Pid ! ko
                    end
            end,

            listener_loop(Current_nodes, Objects_dict, Neighbour);

        % Updates the list of active nodes
        {update_nodes_list, Nodes} ->
            io:format("Nodes list updated: ~w~n", [Nodes]),
            % Check the number of active nodes in order to define the deterinate the
            % two adjacent nodes to be asked when the client nedds a shared object
            if
                % You are the only one, forever alone :'(
                length(Nodes) == 1 ->
                    New_neighbour = false;
                % One or more neighbours, get the adjacents
                true ->
                    Pos = get_node_pos(Nodes, 1),
                    if
                        Pos == length(Nodes) ->
                            New_neighbour = lists:nth(1, Nodes);
                        true ->
                            New_neighbour = lists:nth(Pos + 1, Nodes)
                    end
            end,
            io:format("New neighbour: ~w~n", [New_neighbour]),
            listener_loop(Nodes, Objects_dict, New_neighbour)
    end.
    

%%
%% Main process, get the parameters from the terminal, and after check
%% if all is correct, launch the messages listener
%%
start() ->
    Listener_pid = spawn(bootstrap, listener_loop, [[], dict:new(), false]),

    % Using the name@domain to create a global uniqe identifier, we can't
    % have more than a node on the same machine with the same name
    Check_uniq_id = atom_to_list(node()),
    io:format("UID ~w~n", [list_to_atom(Check_uniq_id)]),
    {manager, ?MASTER_MANAGER} ! {add_node, Listener_pid, Check_uniq_id},
    keep_alive(Check_uniq_id),

    exit(normal).
