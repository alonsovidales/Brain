%%
%% @author Alonso Vidales <alonso.vidales@tras2.es>
%% @version 0.1
%% @since 2012-10-02
%%

%% The nodes will send each time_to_refesh_node_ms miliseconds a signal to keep it alive
-define(TIME_TO_REFRESH_NODE_MS, 5000).
%% Manager nodes tuple, the fists one will be considerer as the master of the nodes
-define(MANAGER_NODES, {master1, master2}).
