%%
%% @author Alonso Vidales <alonso.vidales@tras2.es>
%% @version 0.1
%% @since 2012-10-02
%%

%% The nodes will send each time_to_refesh_node_ms miliseconds a signal to keep it alive
-define(TIME_TO_REFRESH_NODE_MS, 10000).
%% Manager master node, will be used as primary
-define(MASTER_MANAGER, manager@tras2.es).
%% Slave node
-define(SLAVE_MANAGER, manager@tras2.es).
