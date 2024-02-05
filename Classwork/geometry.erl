% Examples from Armstrong, Ch. 4.
% area() is first example of function
%
% First, in erlang shell, define a rectangle and square:
% > Rect = {rectangle, 10, 5}.
% > Sq   = {square, 3}.
%
% Then take them apart with patterns:
% {rectangle, W, H} = Rect.
% {square, Side} = Sq.
%
% Then show definition of area below, and note how heads of function
% clauses are based on the patterns we used in the shell.
%
% Run code.  I think showing a bad call, is a good, idea, too:
% geometry:area({foo}).
%
-module(geometry).
-export([area/1]).
-export([test/0]).

area({rectangle, Width, Height}) -> Width   * Height;
area({square,    SideLen})       -> SideLen * SideLen.

% Armstrong adds test cases to module immediately:

test() ->
    12  = area({rectangle,  3, 4}),
    144 = area({square,    12}),
    tests_worked.

%
% Erlang has a more thorough "common or unit test framework," and
% Armstrong refers reader to on-line language documentation.  See
% EUnit.  It's actually pretty easy to use!
%

