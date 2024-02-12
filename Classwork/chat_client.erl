-module(chat_client).

-export([join/3]).


%%% Client Side
join(ChatName, NodeName, User) -> 
        {ChatName, NodeName} ! {join, {User, 
                spawn(fun chat_client:message_listen/0)}},
        message_reader(ChatName, NodeName, User).

message_listen() ->
        receive 
                {message, Name, Message} -> 
                        io:format("~s : ~s~n", [Name, Message])
        end.

message_reader(ChatName, NodeName, User) -> 
        Message = io:get_line(),
        {ChatName, NodeName} ! {message, {User, self()}, Message},
        message_reader(ChatName, NodeName, User).


% start(Name) -> 
%         register(Name, spawn(distributedChatServer, ))

% receive_message() -> 
%         receive 
%                 {message, _From, Message} -> println(Message)
%         end, 
%         receive_message().


% send_message(Message, Destination) -> 
%         Destination ! {message, self(), Message}.