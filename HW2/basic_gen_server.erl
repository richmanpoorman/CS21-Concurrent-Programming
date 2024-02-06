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
start(Module) -> spawn(basic_gen_server, loop, [Module, callback:init()]).

% Name    : loop
% Purpose : Runs the server
% Params  : (atom Module)        The server module to run
%           (state CurrentState) The state at the start of the iteration
% Return  : (None)
loop(Module, CurrentState) -> 
    NextState = 
        receive 
            {call, Pid, Request} -> 
                {reply, Reply, NewState} = 
                    handle_call(Request, Pid, CurrentState),
                Pid ! {call, self(), Reply},
                NewState 
            {cast, Pid, Request} -> 
                {noreply, NewState} = handle_cast(Request, CurrentState),
                NewState
            {code_swap, NewState} -> 
                {ok, NewState} = handle_swap_code(CurrentState),
                NewState 
        end,
    loop(Module, NewState).

    

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
% Purpose : 
% Params  : 
% Return  : 
swap_code(Pid, NewCallBackModule)