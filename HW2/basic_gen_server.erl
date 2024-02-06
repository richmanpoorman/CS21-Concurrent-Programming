% Name    : basic_gen_server
% Purpose : Module of generic server callbacks and functions
-module(basic_gen_server).
-export([start/1, call/2, cast/2, swap_code/2]).

-export([loop/2]).

%%% SERVER SIDE %%%

% Name    : start 
% Purpose : Starts the server
% Params  : (atom Module) Starts the server using the given module
% Return  : (pid) The Pid of the new running server
start(Module) -> spawn(basic_gen_server, loop, [Module, Module:init()]).

% Name    : loop
% Purpose : Runs the server
% Params  : (atom Module)        The server module to run
%           (state CurrentState) The state at the start of the iteration
% Return  : (None)
loop(Module, CurrentState) -> 
    {NextModule, NextState} = 
        receive 
            {call, Pid, Request} -> 
                {reply, Reply, NewState} = 
                    Module:handle_call(Request, Pid, CurrentState),
                Pid ! {call, self(), Reply},
                {Module, NewState};
            {cast, _Pid, Request} -> 
                {noreply, NewState} = 
                    Module:handle_cast(Request, CurrentState),
                {Module, NewState};
            {swap_code, _Pid, NewModule} -> 
                {ok, NewState} = 
                    Module:handle_swap_code(CurrentState),
                {NewModule, NewState} 
        end,
    loop(NextModule, NextState).

    

%%% CLIENT SIDE %%%

% Name    : call 
% Purpose : Sends a synchronous Request to the server at Pid, 
%           and returns the reply
% Params  : (pid Pid)         The Pid where the server is located
%           (request Request) The request to send to the server
% Return  : (reply) The answer from the server
call(Pid, Request) -> 
    Pid ! {call, self(), Request},
    receive 
        {call, Pid, Reply} -> Reply
    end. 

% Name    : cast
% Purpose : Sends an asynchronous Request to the Server
% Params  : (pid Pid)         The Pid where the server is located
%           (request Request) The request to send to the server
% Return  : ok
cast(Pid, Request) -> 
    Pid ! {cast, self(), Request},
    ok.

% Name    : swap_code
% Purpose : Sends an asynchronous Request to swap to the new module
% Params  : (pid Pid)                The Pid where the server is located
%           (atom NewCallBackModule) The new module to swap to 
% Return  : ok
swap_code(Pid, NewCallBackModule) -> 
    Pid ! {swap_code, self(), NewCallBackModule},
    ok.