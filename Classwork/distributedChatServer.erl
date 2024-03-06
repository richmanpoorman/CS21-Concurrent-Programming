-module(distributedChatServer).

-export([start/1, join/2, join/3]).

-export([loop/1]).

%%% Server %%%
start(ServerName) -> 
        register(ServerName, spawn(distributedChatServer, loop, []))

loop(Subscribers) -> 
        NextSubscribers = 
                receive
                        {message, {Name, _Pid}, Message} ->
                                send_messages(Name, Message, Subscribers),
                                Subscribers;
                        {join, {Name, Pid}} -> 
                                [{Name, Pid} | NextSubscribers]
                end,
        loop(NextSubscribers).

send_messages(_Name, _Message, []) -> ok;
send_messages(Name, Message, [{Subscriber, Pid} | Next]) -> 
        Pid ! {message, Name, Message}.

%%% Client Side
join(ChatName, NodeName, User) -> 
        {ChatName, NodeName} ! {join, {User, spawn(message_listen)}},
        message_reader(ChatName, NodeName, User).

message_listen() ->
        receive 
                {message, Name, Message} -> 
                        io:format("~s : ~s\n", [Name, Message])
        end.

message_reader(ChatName, NodeName, User) -> 
        Message = io:get_line(),
        {ChatName, NodeName} ! {message, {User, self()}, Message},
        message_reader().


% start(Name) -> 
%         register(Name, spawn(distributedChatServer, ))

% receive_message() -> 
%         receive 
%                 {message, _From, Message} -> println(Message)
%         end, 
%         receive_message().


% send_message(Message, Destination) -> 
%         Destination ! {message, self(), Message}.