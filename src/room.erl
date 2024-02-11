-module(room).
-export([new/1, get_id/1, get_owner/1, join/2]).

new(Owner) ->
    {room, erlang:unique_integer([positive, monotonic]), Owner, [Owner]}.

get_id({room, Id, _, _}) ->
    Id.

get_owner({room, _, Owner, _}) ->
    Owner.

join(User, {room, Id, Owner, Members}) ->
    {room, Id, Owner, [User | Members]}.