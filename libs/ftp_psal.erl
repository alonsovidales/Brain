%%
%% Persistant storage access lyaer for FTP storage systems
%%
%% @author Alonso Vidales <alonso.vidales@tras2.es>
%% @version 0.1
%% @since 2012-10-21
%% @see config.hrl
%%

-module(psal).

-export([
    get_file_content/1,
    create_or_update_file/2,
    del_file/1]).

-include("../shared/config.hrl").

%%
%% Try to open a connection with the FTP server, and returns the PID of the
%% connection of an atom false if something was wrong
%%
%% @return pid The PID of the FTP connection, atom false is the connection can't be opened
%%
connect() ->
    case ftp:open(?FTP_HOST, {port, ?FTP_PORT}) of
        {ok, Pid} ->
            ftp:user(Pid, ?FTP_UNAME, ?FTP_PASS),

            case ftp:cd(Pid, ?BASE_DIR) of
                ok ->
                    Pid;
                {error, Reason} ->
                    io:format("Error trying to change to directory \"~w\"~n", [?BASE_DIR]),
                    false
            end,

        {error, Reason} ->
            io:format("Error opening connection with FTP server \"~w\" on port: \"~w\"~n", [?FTP_HOST, ?FTP_PORT]),
            false
    end.

%%
%% Creates a file, or in case of the file previously exists,
%% overwrite this with the given content
%%
%% @param File_name string The name of the file to be written
%% @param Content bin The content to store in the file
%% @return bool Atom false if something was wrong of true if the written was success
%%
create_or_update_file(File_name, Content) ->
    Pid = connect()
    if
        Pid == false ->
            false;

        true ->
            ftp:send_bin(Pid, Content, File_name),
            ftp:close(Pid)
    end.
    
%%
%% Removes a file on the ?BASE_DIR directory
%%
%% @param File_name string The name of the file to be deleted
%% @return bool Atom false if something was wrong of true if the file was deleted
%%
del_file(File_name) ->
    Pid = connect()
    if
        Pid == false ->
            false;

        true ->
            ftp:delete(Pid, File_name),
            ftp:close(Pid),
            true
    end.

%%
%% Returns the content of a file on the ?BASE_DIR directory as it.
%% If the file doesn't exists, return an atom false
%%
%% @param File_name string The name of the file to be read on ?BASE_DIR
%% @return bin The binary content of the file, or an atom false
%%
get_file_content(File_name) ->
    Pid = connect()
    if
        Pid == false ->
            false;

        true ->
            Return = case ftp:recv_bin(Pid, File_name) of
                {ok, Content} ->
                    Content;
                {error, _Reason} ->
                    false
            end,
                    
            ftp:close(Pid),

            Return
    end.
