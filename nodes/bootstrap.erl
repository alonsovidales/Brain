%%
%% Run this file on each master node
%%
%% @author Alonso Vidales <alonso.vidales@tras2.es>
%% @version 0.1
%% @since 2012-10-02
%% @see config.hrl
%%

-module(bootstrap).
-export([
    start/0,
    listener_loop/3,
    object_record_control_loop/2,
    persist_object/3,
    get_object_form_persistance_layer/3]).
-include("../shared/config.hrl").
-compile("../shared/vendor/erls3.erl").

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

%%
%% Controls if an object is in use, and if it isnot accessed during
%% a TIME_TO_CONSIDERER_OBJECT_INACTIVE period of time, persist the
%% object on S3
%%
%% @param Main_loop_pid Pid The Pid of the main loop who will exectue the
%%                          persistance migration to S3 in ordedr to keep
%%                          the consistency
%% @param Object_id List The object id to be monitorized
%%
object_record_control_loop(Main_loop_pid, Object_id) ->
    receive
        used ->
            object_record_control_loop(Main_loop_pid, Object_id)
    after ?TIME_TO_CONSIDERER_OBJECT_INACTIVE ->
        Main_loop_pid ! {p, Object_id}
    end.


persist_object(Retr_pid, Object_id, {ok, Value}) ->
    io:format("Storing on S3 object \"~w\" with value \"~w\"~n", [list_to_atom(Object_id), list_to_atom(Value)]),
    Retr_pid ! {remove_object_from_dict, Object_id}.

get_object_form_persistance_layer(Pid_to_add_on_mem, Retr_pid, Object_id) ->
    io:format("Getting object from S3 with id: \"~w\"~n", [list_to_atom(Object_id)]),
    % TODO: Check if the object exists in case of don't exists, return ko to Retr_pid
    Value = "This is the value from S3...",
    Pid_to_add_on_mem ! {s, Object_id, Value},
    Retr_pid ! {ok, Value}.

listener_loop(Current_nodes, Objects_dict, Neighbour) ->
    io:format("Listener loop, Current_nodes: ~w~n", [Current_nodes]),
    receive
        % Save a new object on the local memory dictionary
        {s, Object_id, Value} ->
            io:format("Save Object Key: \"~w\" Value: \"~w\"~n", [list_to_atom(Object_id), list_to_atom(Value)]),

            % Check if the observer process is yet launched for this object, and in that case,
            % don't launch it again, only modify the object
            Observer_pid = whereis(list_to_atom(string:concat("observe_", Object_id))), 
            if
                Observer_pid == undefined ->
                    register(
                        list_to_atom(string:concat("observe_", Object_id)),
                        spawn(bootstrap, object_record_control_loop, [self(), Object_id])),
        	    % Add the object to the dictionary, the first param of the tuple indicates if are not modified will not be stored
	            listener_loop(Current_nodes, dict:store(Object_id, {false, Value}, Objects_dict), Neighbour);
		true ->
                    % Refresh the observer timmer
                    Observer_pid ! used,
                    % Modify the object, set the modify flag to true this meand that this object should to be stored
	            listener_loop(Current_nodes, dict:store(Object_id, {true, Value}, Objects_dict), Neighbour)
            end,

        % Get an object from memory with Object_id as key, if Consistency is true,
        % the system will askto all the nodes until find it.
        {g, Consistency, Object_id, Pid, Origin_pid} ->
            io:format("Looking for global object ~w~n", [list_to_atom(Object_id)]),
            case dict:find(Object_id, Objects_dict) of
                {ok, {Modified, Value}} ->
                    io:format("Value found for key ~w: \"~w\"~n", [list_to_atom(Object_id), list_to_atom(Value)]),
                    Pid ! {ok, Value},
                    % Refresh the observer timmer
                    Observer_pid = whereis(list_to_atom(string:concat("observe_", Object_id))),
                    Observer_pid ! used;
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
                        Consistency and (Neighbour_id /= Origin_node) ->
                            io:format("Key ~w not found, seaching on neightbour~n", [list_to_atom(Object_id)]),
                            Neighbour_pid ! {g, Consistency, Object_id, Pid, Origin_node};
                            % Don't use consistency, or the object is not in a neghtbour,
                            % get it from the persistance layer
                        true ->
                            io:format("Key ~w not found, try to get it from S3~n", [list_to_atom(Object_id)]),
                            spawn(bootstrap, get_object_form_persistance_layer, [self(), Pid, Object_id]),
                            Pid ! ko
                    end
            end,

            listener_loop(Current_nodes, Objects_dict, Neighbour);

        % Dumps the object to the persistance layer in this case S3
        {p, Object_id} ->
            io:format("Move object with id \"~w\" to the persistance layer~n", [list_to_atom(Object_id)]),
            % Creating a new process in order to don't block the system dump the object to S3
            % this call will call to this loop after sotre the object in S3 to keep the consistency
            spawn(bootstrap, persist_object, [self(), Object_id, dict:find(Object_id, Objects_dict)]),
            listener_loop(Current_nodes, Objects_dict, Neighbour);

        % Called after the store to S3 is done
        {remove_object_from_dict, Object_id} ->
            listener_loop(Current_nodes, dict:erase(Object_id, Objects_dict), Neighbour);

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
