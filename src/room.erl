-module(room).
-export([new/3, get_id/1, get_owner/1, get_members_names/1, get_members_sockets/1, get_accessibility/1, join/3, leave/3]).

new(OwnerName, Socket, IsPrivate) ->
    {room, erlang:unique_integer([positive, monotonic]), {OwnerName, Socket}, [{OwnerName, Socket}], IsPrivate}.

get_id({room, Id, _, _, _}) ->
    Id.

get_owner({room, _, {OwnerName, _}, _, _}) ->
    OwnerName.

get_members_names({room, _, _, Members, _}) ->
    [UserName || {UserName, _} <- Members].

get_members_sockets({room, _, _, Members, _}) ->
    [Socket || {_, Socket} <- Members].

get_accessibility({room, _, _, _, IsPrivate}) ->
    IsPrivate.

join(UserName, Socket, {room, Id, Owner, Members, IsPrivate}) ->
    case lists:member({UserName, Socket}, Members) of
        true ->
            {room, Id, Owner, Members, IsPrivate};
        false ->
            % Add a user only if it's not already Member
            {room, Id, Owner, [{UserName, Socket} | Members], IsPrivate}
    end.

leave(UserName, Socket, {room, Id, Owner, Members, IsPrivate}) ->
    UpdatedMembers = lists:delete({UserName, Socket}, Members),
    {room, Id, Owner, UpdatedMembers, IsPrivate}.