% Name    : chat.erl
% Purpose : Runs a distributed chat server with rooms that people can join

-module(chat).
-behavior(gen_server).
%% API

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-record(state, {users}).
-define(chat, ?MODULE).

%%% CLIENT FUNCTIONS %%% 
-export([join_room/3]).

-export([stop/1, start_link/1]).

% Name    : join_room
% Purpose : Joins the chat room given the server, room id, and user name; 
%           starts up the processes to send and receive messages, as well 
%           as joining the room on the server side, and leaving when finished
% Params  : (node ServerNode) The erlang node that the chat room is hosted on
%           (atom Room)       The process ID of the room on the server
%           (atom Name)       The user name when joining the room
% Return  : ok 
join_room(ServerNode, Room, Name) ->
    Listener = spawn(fun () -> receive_messages() end), 
    Server   = {Room, ServerNode},
    gen_server:cast(Server, {subscribe, {Name, Listener}}),
    send_messages(Server, Name), 
    gen_server:cast(Server, {unsubscribe, {Name, Listener}}),
    Listener ! {done},
    ok.

% Name    : send_messages
% Purpose : The client-side message sender, which sends the message to the 
%           server along with the user's name 
% Params  : (server_id Server) The room to send messages to
%           (string    Name)   The name of the user sending the message
% Return  : ok
send_messages(Server, Name) -> 
    Input = io:get_line("Enter Message: "),
    case Input of 
        "--quit\n" -> ok; 
        "--list\n" -> 
            List = gen_server:call(Server, list),
            io:format("~p~n", [List]),
            send_messages(Server, Name);
        Message -> 
            gen_server:cast(Server, {message, {Name, Message}}),
            send_messages(Server, Name)
    end, 
    ok. 

% Name    : receive_messages
% Purpose : Listens for messages coming from the server, and prints them out
%           for the client
% Params  : (None)
% Return  : ok
receive_messages() ->
    receive 
        {message, {User, Message}} ->
            io:format("\n~s: ~s", [User, Message]),
            receive_messages();
        {stop, Message} -> 
            io:format("Server is done because: ~p \n Please --quit\n", 
                [Message]),
            ok;
        {done} ->
            ok
    end,
    ok. 


%%% SERVER ADMINISTRATION %%% 

% Name    : stop
% Purpose : Ends the given chat room name on the server-side
% Params  : (atom Name) The name of the chat room
% Return  : ok
stop(Name) ->
    gen_server:call(Name, stop),
    ok.

% Name    : start_link
% Purpose : Starts the chat room, which is linked to the supervisor process
% Params  : (atom Name) The name of the chat room
% Return  : (N/A)
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []),
    ok.

%%% SERVER FUNCTIONS %%%%

% Name    : The initial state of any given chat room
% Purpose : Sets the initial values of the chat room
% Params  : (... args) The arguments needed to start the chat room
% Return  : {ok, InitialState}
init(_Args) ->
    {ok, #state{users = maps:new()}}.

% Name    : handle_call
% Purpose : Takes synchronous call requests, and responds
% Params  : (request Request) The type of message being asked for 
%           (pid     From)    The Pid of the process that send the request 
%           (state   State)   The state of the server when the call is made
% Return  : (reply) The reply from the server, with the next state
handle_call(stop, _From, State) ->
    Users     = State#state.users, 
    UserPids  = maps:values(Users),
    SendEnd   = fun (UserPid) -> UserPid ! {stop} end,
    lists:foreach(SendEnd, UserPids),
    {stop, normal, stopped, State};
handle_call(list, _From, State) ->
    Users     = State#state.users, 
    UserNames = maps:keys(Users),
    {reply, UserNames, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

% Name    : handle_cast
% Purpose : Takes asynchronous cast requests
% Params  : (request Request) The request being made to the server
%           (state   State)   The state of the server when the cast is made
% Return  : (noreply) The next state of the server
handle_cast({subscribe, {Name, Listener}}, State) -> 
    Users    = State#state.users,
    NewUsers = maps:put(Name, Listener, Users),
    NewState = #state{users = NewUsers},
    {noreply, NewState};
handle_cast({unsubscribe, {Name, _Listener}}, State) -> 
    Users    = State#state.users,
    NewUsers = maps:remove(Name, Users),
    NewState = #state{users = NewUsers},
    {noreply, NewState};
handle_cast({message, {Name, Message}}, State) -> 
    Users    = State#state.users, 
    UserPids = maps:values(Users), 
    SendMsg  = fun (UserPid) -> UserPid ! {message, {Name, Message}} end,
    lists:foreach(SendMsg, UserPids),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

% Name    : terminate
% Purpose : terminates the server
% Params  : (atom  Reason) The reason to close the server
%           (state State)  The state of the server when the termination is made
% Return  : ok
terminate(Reason, State) ->
    Users    = State#state.users, 
    UserPids = maps:values(Users),
    SendMsg  = fun (UserPid) -> UserPid ! {stop, Reason} end, 
    lists:foreach(SendMsg, UserPids),
    ok.