% Written by Caleb Helbling
% Last updated Oct 10, 2014

-module(name_server).
-export([init/0, add/3, whereis/2, handle_cast/2,
         handle_call/3, handle_swap_code/1]).

%% client routines

add(ServerPid, Person, Place) ->
    basic_gen_server:cast(ServerPid, {add, Person, Place}).

whereis(ServerPid, Person) ->
    basic_gen_server:call(ServerPid, {whereis, Person}).

%% callback routines

init() ->
    maps:new().

handle_cast({add, Person, Place}, State) ->
    NewState = maps:put(Person, Place, State),
    {noreply, NewState}.

handle_call({whereis, Person}, _From, State) ->
    Reply = case maps:find(Person, State) of
        {ok, Place} -> Place;
        error -> error
    end,
    NewState = State,
    {reply, Reply, NewState}.

handle_swap_code(State) ->
    {ok, State}.
