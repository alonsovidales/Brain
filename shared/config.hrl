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
%% Max time to keep an object on memory withouth be used
-define(TIME_TO_CONSIDERER_OBJECT_INACTIVE, 10000).

%% Amazon S3 access config
-define(ACCESS_KEY, "MY_ACCESS_KEY").
-define(SECRET_KEY, "MY_SECRET_KEY").
-define(AWS_S3_HOST, "s3.amazonaws.com").
-define(TIMEOUT, infinity).
-define(CHUNK_SIZE, 1024 5).
