-module(add_object_test).
-export([start/0]).
-include("../shared/config.hrl").

get_object_from_node(Object_id, Pid, Consistency) ->
    Pid ! {g, Consistency, Object_id, self(), null},

    receive
        {ok, Result} ->
            Return = Result;
        Value ->
            Return = Value
    end,
    Return.


add_object_to_first_object([Node | _Rest], Object_id, Value) ->
    {Node_id, Node_pid} = Node,
    io:format("Adding Object \"~w\" to Node \"~w\" with value: \"~w\"~n", [
        list_to_atom(Object_id),
        Node_id,
        list_to_atom(Value)]),
    Node_pid ! {s, Object_id, Value}.

get_object_asking_to_first_node([Node | _Rest], Object_id) ->
    {Node_id, Node_pid} = Node,
    io:format("Getting Object \"~w\" from Node \"~w\": ~w~n", [
        list_to_atom(Object_id),
        Node_id,
        list_to_atom(get_object_from_node(Object_id, Node_pid, true))]).

try_to_get_object_not_defined_object([Node | _Rest], Object_id) ->
    {Node_id, Node_pid} = Node,
    io:format("Getting Object \"~w\" from Node \"~w\" ~w~n", [
        list_to_atom(Object_id),
        Node_id,
        list_to_atom(get_object_from_node(Object_id, Node_pid, true))]).

get_object_asking_to_last_node_with_consistency(Nodes, Object_id) ->
    {Node_id, Node_pid} = lists:last(Nodes),
    io:format("Getting Object \"~w\" from last Node \"~w\" ~w~n", [
        list_to_atom(Object_id),
        Node_id,
        list_to_atom(get_object_from_node(Object_id, Node_pid, true))]).

get_object_withouth_consistency_from_first([Node | _Rest], Object_id) ->
    {Node_id, Node_pid} = Node,
    io:format("Getting Object without consistency \"~w\" from\"~w\" ~w~n", [
        list_to_atom(Object_id),
        Node_id,
        list_to_atom(get_object_from_node(Object_id, Node_pid, false))]).

get_object_withouth_consistency_from_last(Nodes, Object_id) ->
    {Node_id, Node_pid} = lists:last(Nodes),
    io:format("Getting Object without consistency \"~w\" from last Node \"~w\" ~w~n", [
        list_to_atom(Object_id),
        Node_id,
        get_object_from_node(Object_id, Node_pid, false)]).

start() ->
    {manager, ?MASTER_MANAGER} ! {get_nodes, self()},

    receive
        Nodes_list ->
            Nodes = Nodes_list
    end,

    io:format("Current list: ~w~n", [Nodes]),

    add_object_to_first_object(Nodes, "test_object", "This is the value!!!!"),

    get_object_asking_to_first_node(Nodes, "test_object"),

    get_object_asking_to_last_node_with_consistency(Nodes, "test_object"),

    get_object_withouth_consistency_from_first(Nodes, "test_object"),

    get_object_withouth_consistency_from_last(Nodes, "test_object"),

    try_to_get_object_not_defined_object(Nodes, "test_aaaaa"),

    % get_object_with_consistency_from_all_the_nodes(Nodes, "test_aaaaa"),

    exit(normal).
