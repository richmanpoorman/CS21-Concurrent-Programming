-module(chat_server).
-export([start/1]).

-export([loop/1]).

%%% Server %%%
start(ServerName) -> 
        register(ServerName, spawn(chat_server, loop, [[]])).

loop(Subscribers) -> 
        NextSubscribers = 
                receive
                        {message, {Name, _Pid}, Message} ->
                                send_messages(Name, Message, Subscribers),
                                Subscribers;
                        {join, {Name, Pid}} -> 
                                [{Name, Pid} | Subscribers]
                end,
        loop(NextSubscribers).

send_messages(_Name, _Message, []) -> ok;
send_messages(Name, Message, [{Subscriber, Pid} | Next]) -> 
        Pid ! {message, Name, Message},
        send_messages(Name, Message, Next).
