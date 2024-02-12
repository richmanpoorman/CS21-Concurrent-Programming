-behaviour(gen_server).

%% API
-export([start/1, stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {members}).

%%% CLIENT FUNCTIONS %%% 
-export([join_room/3, subscribe/3, unsubscribe/3, post/2, list/2]).

join_room(ServerNode, Room, Name) ->
    UserPid  = self(),
    Listener = spawn(receive_messages(ServerNode, Room, UserPid) end), 
    Server   = {Room, ServerNode},
    gen_server:cast(Server, {subscribe, {Name, Listener}}),
    send_messages(Server, Name), % Starts recieve loop
    gen_server:cast(Server, {unsubscribe, {Name, Listener}}), % Leave when ending the loop
    Listener ! {done, Me},
    ok.

send_messages(Server, Name) -> 
    Input = io:get_line("Enter Message: "),
    case Input of 
        "--quit\n" -> ok; 
        "--list\n" -> 
            List = gen_server:call(Server, {list, Name}),
            io:format("~w~n", [List]);
            send_messages(Server, Name);
        Message -> gen_server:cast(Server, {message, Message})
    end. 

recieve_messages(ServerNode, Room, UserPid) ->
    receive 
        {message, {User, Message}} ->
            io:format("~w: ~w~n", [User, Message]),
            recieve_messages(ServerNode, Room, UserPid);
        {stop, _ServerNode, _Room} ->
            ok;
        {done, _Me} ->
            ok. 
    end. 


%%% SERVER ADMINISTRATION %%% 
stop(Name) ->
    gen_server:call(Name, stop).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%%% SERVER FUNCTIONS %%%%
init(_Args) ->
    {ok, #state{dummy=1}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
