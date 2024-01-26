-module(ps01).
-export([is_homogenous/1, get_roots/3]).

% Name    : is_homogenous
% Purpose : Returns true if all of the elements of the list are the same,
%           false otherwise
% Params  : (List[]) The list to check the elements of
% Return  : (boolean) Whether all of the elements are the same or not
is_homogenous([])  -> true;
is_homogenous([_]) -> true; 
is_homogenous([Element | Next]) -> matches_with(Element, Next).

% Name    : matches_with
% Purpose : Returns true if all of the elements of the list match the given 
%           element, false if they do not
% Params  : (auto)   The element of the list to check against 
%           (List[]) The list to check the elements of
% Return  : (boolean) Whether all of the elements match or not
matches_with(Element, [Element])        -> true;
matches_with(Element, [Element | Next]) -> matches_with(Element, Next);
matches_with(Element, [_])              -> false.

% Name    : get_roots
% Purpose : Returns the list of the possible solutions to the associated
%           quadratic formula ax^2 + bx + c, or error if a = 0
% Params  : (Number) The value of the 'a' coefficient
%           (Number) The value of the 'b' coefficient
%           (Number) The value of the 'c' coefficient
% Return  : (boolean) Whether all of the elements match or not
get_roots(0, _, _)                         -> error;
get_roots(A, B, C) when (B*B - 4*A*C)  < 0 -> [];
get_roots(A, B, C) when (B*B - 4*A*C) == 0 -> [(-B / (2 * A))];
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