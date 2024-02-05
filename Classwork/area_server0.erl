%%
%% From Armstrong, Ch. 4
%% 
%% with some minor edits from Mark A. Sheldon, Tufts University, Fall 2014
%%

-module(area_server0).
-export([loop/0]).

%%
%% The main server loop
%%
loop() ->
    receive
        {rectangle, Width, Height} -> io:format("Area of rectangle is ~p~n",
                                                [Width * Height]);
        {square, Side}             -> io:format("Area of square is ~p~n",
                                                [Side * Side])
    end,
    loop().

