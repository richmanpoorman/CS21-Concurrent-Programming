-module(ps01).
-export([is_homogeneous/1, get_roots/3, ints_from_to/2, evens_odds/1]).

% Name    : is_homogeneous
% Purpose : Returns true if all of the elements of the list are the same,
%           false otherwise
% Params  : (List[]) The list to check the elements of
% Return  : (boolean) Whether all of the elements are the same or not
is_homogeneous([])               -> true;
is_homogeneous([_])              -> true; 
is_homogeneous([Element | Next]) -> matches_with(Element, Next).

% Name    : matches_with
% Purpose : Returns true if all of the elements of the list match the given 
%           element, false if they do not
% Params  : (auto)   The element of the list to check against 
%           (List[]) The list to check the elements of
% Return  : (boolean) Whether all of the elements match or not
matches_with(_Element, [])              -> true;
matches_with(Element, [Element | Next]) -> matches_with(Element, Next);
matches_with(_Element, [_ | _])         -> false.

% Name    : get_roots
% Purpose : Returns the list of the possible solutions to the associated
%           quadratic formula ax^2 + bx + c, or error if a = 0
% Params  : (Number) The value of the 'a' coefficient
%           (Number) The value of the 'b' coefficient
%           (Number) The value of the 'c' coefficient
% Return  : (boolean) Whether all of the elements match or not
get_roots(A, _, _) when A == 0             -> error;
get_roots(A, B, C) when (B*B - 4*A*C)  < 0 -> [];
get_roots(A, B, C) when (B*B) == (4*A*C)   -> [(-B / (2 * A))];
get_roots(A, B, C) when (B*B - 4*A*C)  > 0 -> [
    ((-B - math:sqrt(B*B - 4*A*C)) / (2 * A)),
    ((-B + math:sqrt(B*B - 4*A*C)) / (2 * A))
].

% Name    : ints_from_to
% Purpose : Returns a list of integers from Low (inclusive) to High (exclusive)
% Params  : (Number) The Low value to start from
%           (Number) The High value to go to
% Return  : (List[Number]) The list of numbers [Low, High)
ints_from_to(A, B) when A >= B -> [];
ints_from_to(A, B) when A < B  -> [A | ints_from_to(A + 1, B)].

% Name    : evens_odds
% Purpose : Returns a tuple separating the even and odd numbers into their
%           own lists
% Params  : (List[Number]) List to separate into even and odd lists
% Return  : ({List, List}) The list of even numbers and list of odd numbers
evens_odds(List) -> get_even_odds(List, [], []).

% Name    : get_evens_odds
% Purpose : Returns a tuple separating the even and odd numbers into their
%           own lists, keeping track using parameters
% Params  : (List[Number]) List to separate into even and odd lists
%           (List[Number]) List of Even numbers accumulated
%           (List[Number]) List of Odd numbers accumulated
% Return  : ({List, List}) The list of even numbers and list of odd numbers
get_even_odds([], Evens, Odds)                           -> {Evens, Odds};
get_even_odds([X | Next], Evens, Odds) when X rem 2 == 0 -> 
    get_even_odds(Next, [X | Evens], Odds);
get_even_odds([X | Next], Evens, Odds) when X rem 2 == 1 ->
    get_even_odds(Next, Evens, [X | Odds]).