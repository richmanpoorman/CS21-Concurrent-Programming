% Module  : concurrency
% Purpose : Parallel operations for mapping, merging, and sorting

-module(concurrency).
-export([parallel_map/2, merge/2, merge_sort/1]).

-export([parallel_map_call/3, parallel_merge_sort/2]).

% Name    : parallel_map(Fun, List)
% Purpose : Returns a list whose elements are the values of the elements of 
%           List after going through the function Fun, running concurrently
% Params  : (function/1 Fun) The function run on each element 
%           (list List)      The list to perform Fun on each element
% Return  : (list) A list with the elements of List run through Fun
parallel_map(_Fun, []) -> [];
parallel_map(Fun, List) -> 
    ParallelCall = spawn(concurrency, parallel_map_call, [self(), Fun, List]),
    receive 
        {ParallelCall, Response} -> Response
    end. 

% Name    : parallel_map_call(ReturnPid, Fun, List)
% Purpose : Performs the function Fun on each element of the list concurrently
%           and sends a message with the mapped list to the ReturnPid
% Params  : (pid ReturnPid)  The Pid address to send the message back to
%           (function/1 Fun) The function run on each element 
%           (list List)      The list to perform Fun on each element
% Return  : (list) A list with the elements of List run through Fun
parallel_map_call(ReturnPid, _Fun, []) ->
    ReturnPid ! {self(), []};
parallel_map_call(ReturnPid, Fun, [Value | Next]) ->
    NextPid = spawn(concurrency, parallel_map_call, [self(), Fun, Next]),
    MappedValue = Fun(Value),
    RestOfList =
        receive
            {NextPid, NextList} -> NextList 
        end,
    ReturnPid ! {self(), [MappedValue | RestOfList]}.


% Name    : merge(ListA, ListB)
% Purpose : Takes two sorted lists, and merges the lists while maintaining 
%           being sorted
% Params  : (list ListA) A sorted list of numbers
%           (list ListB) A sorted list of numbers 
% Return  : (list) A sorted list with elements of both ListA and ListB
merge([], ListB) -> ListB; 
merge(ListA, []) -> ListA;
merge([ValA | NextA], [ValB | NextB]) when ValA =< ValB ->
    [ValA | merge(NextA, [ValB | NextB])];
merge([ValA | NextA], [ValB | NextB]) when ValA > ValB ->
    [ValB | merge([ValA | NextA], NextB)].

% Name    : merge_sort(List)
% Purpose : Sorts the elements of a list of numbers using concurrency
% Params  : (list List) The list of numbers to sort
% Return  : (list) The sorted list of numbers
merge_sort([]) -> [];
merge_sort(List) -> 
    ParallelSort = spawn(concurrency, parallel_merge_sort, [self(), List]),
    receive
        {ParallelSort, Response} -> Response 
    end.

% Name    : parallel_merge_sort(ReturnPid, List)
% Purpose : Sorts the lists concurrently, and sending the sorted list to 
%           the return Pid when done
% Params  : (pid ReturnPid) The Pid address to send the message back to
%           (list List)     The list to sort
% Return  : (list) The sorted list
parallel_merge_sort(ReturnPid, []) -> 
    ReturnPid ! {self(), []};
parallel_merge_sort(ReturnPid, List) ->
    Length = length(List, 0),
    [ListA, ListB] = lists:split(Length / 2, List),
    ListAPid = spawn(concurrency, parallel_merge_sort, [self(), ListA]),
    ListBPid = spawn(concurrency, parallel_merge_sort, [self(), ListB]),
    SortedA = 
        receive
            {ListAPid, SortedListA} -> SortedListA 
        end,
    SortedB =
        receive
            {ListBPid, SortedListB} -> SortedListB 
        end,
    SortedList = merge(SortedA, SortedB),
    ReturnPid ! {self(), SortedList}. 

% Name    : length(List, Size)
% Purpose : Given an accumulator Size and a list List, the function 
%           returns the length of the list
% Params  : (list List) The list to find the size of
%           (number Size) The accumulator for the size (should be normally
%               given as 0) 
% Return  : (number) The size of the list
length([], Size) -> Size;
length([_ | Tail], Size) -> length(Tail, 1 + Size).