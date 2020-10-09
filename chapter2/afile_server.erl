-module(afile_server).
-export([start/0, start/1, loop/1]).

start() ->
    spawn(afile_server, loop, ["."]).


start(Dir) -> 
    spawn(afile_server, loop, [Dir]).

% 收到 list_dir 就 ls Dir
% 收到 {get_file, File} 就 cat Dir/File
loop(Dir) ->
    receive
        {Client, list_dir} ->
            Client ! {self(), file:list_dir(Dir)};
        {Client, {get_file, File}} ->
            Full = filename:join(Dir, File),
            Client ! {self(), file:read_file(Full)}
    end,
    loop(Dir).
