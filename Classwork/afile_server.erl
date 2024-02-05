%%% afile_server.erl
%%% 
%%% From Joe Armstrong, Programming Erlang:  Software for a Concurrent
%%% World, 2nd edition, Ch. 2, p. 15.
%%%
-module(afile_server).
-export([start/1, loop/1]).

start(Dir) -> spawn(afile_server, loop, [Dir]).

loop(Dir) ->
    receive
        {Client, list_dir} ->
            Client ! {self(), file:list_dir(Dir)},
            loop(Dir);
        {Client, {get_file, File}} ->
            Full = filename:join(Dir, File),
            Client ! {self(), file:read_file(Full)},
            loop(Dir)
    end.


          
