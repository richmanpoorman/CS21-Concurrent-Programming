-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

% Name    : new
% Purpose : Returns a new, empty database
% Params  : None
% Return  : (Database) An empty database
new() -> [].

% Name    : destroy
% Purpose : Clears the given databse
% Params  : (Database) The to database to clear
% Return  : (Database) The database after clearing
destroy(_) -> new().

% Name    : write
% Purpose : Inserts the given key into the database if the key exists, or 
%           creates a new entry in the database if the key does not exist
% Params  : (Key)      The identifier of the data
%           (Element)  The value to store associated with the given key
%           (Database) The database to write to
% Return  : (Database) The updated database after writing
write(Key, Element, [])                -> [{Key, Element}];
write(Key, Element, [{Key, _} | Next]) -> [{Key, Element} | Next];
write(Key, Element, [Entry | Next])    -> [Entry | write(Key, Element, Next)].

% Name    : delete
% Purpose : Removes the given key into the database if the key exists, or 
%           returns the database if the key does not exist
% Params  : (Key)      The identifier of the data
%           (Database) The database to delete from
% Return  : (Database) The updated database after deleting
delete(Key, [])                -> [];
delete(Key, [{Key, _} | Next]) -> Next;
delete(Key, [Entry | Next])    -> [Entry | delete(Key, Next)].

% Name    : read
% Purpose : Gets the data associated with a given key in the database if 
%           the key exists, or returns an error if the key does not exist
% Params  : (Key)      The identifier of the data
%           (Database) The database to read from
% Return  : (Value)    The data associated with the given key
read(Key, [])              -> {error, instance};
read(Key, [{Key, Value}])  -> Value;
read(Key, [{_, _} | Next]) -> read(Key, Next).

% Name    : match
% Purpose : Gets a key which is associated with the given value, or returns 
%           an error if the key does not exist
% Params  : (Value)    The data to find the key of
%           (Database) The database to read from
% Return  : (Key)      The identifier of the data
match(Value, [])              -> {error, instance};
match(Value, [{Key, Value}])  -> Key;
match(Value, [{_, _} | Next]) -> match(Value, Next).