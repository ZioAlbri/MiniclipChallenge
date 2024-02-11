-module(room).
-export([new/1, get_id/1, get_owner/1, get_members/1, join/2, leave/2]).

new(Owner) ->
    {room, erlang:unique_integer([positive, monotonic]), Owner, [Owner]}.

get_id({room, Id, _, _}) ->
    Id.

get_owner({room, _, Owner, _}) ->
    Owner.

get_members({room, _, _, Members}) ->
    Members.

join(User, {room, Id, Owner, Members}) ->
    case lists:member(User, Members) of
        true ->
            {room, Id, Owner, Members};
        false ->
            % Add a user only if it's not already Member
            {room, Id, Owner, [User | Members]}
    end.

leave(User, {room, Id, Owner, Members}) ->
    UpdatedMembers = lists:delete(User, Members),
    {room, Id, Owner, UpdatedMembers}.