%%
%% Run this file on each master node
%%
%% @author Alonso Vidales <alonso.vidales@tras2.es>
%% @version 0.1
%% @since 2012-10-02
%% @see config.hrl
%%

-module(bootstrap).
-export([start/0, listener_loop/2]).
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

listener_loop(Nodes, Objects_dict) ->
    receive
        % Save a new object on the local memory dictionary
        {s, Object_id, Value} ->
            io:format("Save Object Key: \"~w\" Value: \"~w\"~n", [Object_id, Value]),
            listener_loop(Nodes, dict:append(Object_id, Value, Objects_dict));

        % Get an object from memory with Object_id as key, if Consistency is true,
        % the system will askto all the nodes until find it.
        {g, Consistency, Object_id} ->
            io:format("Looking for global object ~w~n", [Object_id]),
            Isset_val = dict:find(Object_id),
            case Isset_val of
                {ok, Value} ->
                    io:format("Value found for key ~w: \"~w\"~n", [Object_id, Value]);
                error ->
                    if
                        Consistency ->
                            io:format("Key ~w not found, seach on partners~n", [Object_id]);
                        true ->
                            io:format("Key ~w not found, don't use consistency~n", [Object_id])
                    end
            end,
            listener_loop(Nodes, Objects_dict);

        % Updates the list of active nodes
        {update_nodes_list, Nodes} ->
            io:format("Nodes list updated: ~w~n", [Nodes]),
            listener_loop(Nodes, Objects_dict)
    end.
    

%%
%% Main process, get the parameters from the terminal, and after check
%% if all is correct, launch the messages listener
%%
start() ->
    Listener_pid = spawn(bootstrap, listener_loop, [[], dict:new()]),

    Check_uniq_id = string:concat(string:concat("check_", atom_to_list(node())), pid_to_list(Listener_pid)),
    io:format("PID ~w~n", [list_to_atom(Check_uniq_id)]),
    {manager, ?MASTER_MANAGER} ! {add_node, Listener_pid, Check_uniq_id},
    keep_alive(Check_uniq_id),

    exit(normal).
