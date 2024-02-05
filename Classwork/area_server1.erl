%%
%% From Armstrong, Ch. 4
%% 
%% with some minor edits from Mark A. Sheldon, Tufts University, Fall 2014
%%

-module(area_server1).
-export([loop/0, rpc/2]).

%% 
%% A function for clients to use
%%
rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        Response -> Response
    end.

%%
%% The main server loop
%%
loop() ->
    receive
        {From, {rectangle, Width, Height}} ->
            From ! Width * Height;
        {From, {square, Side}} ->
            From ! Side * Side;
        {From, {circle, Radius}} ->
            From ! math:pi() * Radius * Radius;
        {From, Other} ->
            From ! {error, Other}
    end,
    loop().


