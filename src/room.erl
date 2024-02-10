-module(room).
-export([new/1, get_id/1, join/2]).

new(Owner) ->
    {room, rand:uniform(1000000), Owner, []}.

get_id({room, Id, _, _}) ->
    Id.

join(User, {room, Id, Owner, Members}) ->
    {room, Id, Owner, [User | Members]}.