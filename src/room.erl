-module(room).
-export([new/2, get_id/1, get_owner/1, get_members/1, join/3, leave/3]).

new(OwnerName, Socket) ->
    {room, erlang:unique_integer([positive, monotonic]), {OwnerName, Socket}, [{OwnerName, Socket}]}.

get_id({room, Id, _, _}) ->
    Id.

get_owner({room, _, {OwnerName, _}, _}) ->
    OwnerName.

get_members({room, _, _, Members}) ->
    [UserName || {UserName, _} <- Members].

join(UserName, Socket, {room, Id, Owner, Members}) ->
    case lists:member({UserName, Socket}, Members) of
        true ->
            {room, Id, Owner, Members};
        false ->
            % Add a user only if it's not already Member
            {room, Id, Owner, [{UserName, Socket} | Members]}
    end.

leave(UserName, Socket, {room, Id, Owner, Members}) ->
    UpdatedMembers = lists:delete({UserName, Socket}, Members),
    {room, Id, Owner, UpdatedMembers}.