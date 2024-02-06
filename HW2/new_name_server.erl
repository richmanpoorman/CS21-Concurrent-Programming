% Written by Caleb Helbling
% Last updated Oct 10, 2014

-module(new_name_server).

-export([init/0, add/3, whereis/2, handle_cast/2,
         handle_call/3, all_names/1, delete/2, handle_swap_code/1]).

%% client routines

add(ServerPid, Person, Place) ->
    basic_gen_server:cast(ServerPid, {add, Person, Place}).

whereis(ServerPid, Person) ->
    basic_gen_server:call(ServerPid, {whereis, Person}).

delete(ServerPid, Person) ->
    basic_gen_server:cast(ServerPid, {delete, Person}).

all_names(ServerPid) ->
    basic_gen_server:call(ServerPid, all_names).

%% callback routines

init() ->
    maps:new().

handle_cast({add, Person, Place}, State) ->
    {noreply, maps:put(Person, Place, State)};
handle_cast({delete, Person}, State) ->
    {noreply, maps:remove(Person, State)}.

handle_call({whereis, Person}, _From, State) ->
    Reply = case maps:find(Person, State) of
        {ok, Place} -> Place;
        error -> error
    end,
    {reply, Reply, State};
handle_call(all_names, _From, State) ->
    {reply, maps:keys(State), State}.

handle_swap_code(State) ->
    {ok, State}.
