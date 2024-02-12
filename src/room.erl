-module(room).
-export([new/3, get_id/1, get_owner/1, get_owner_name/1, get_members_names/1, get_members_sockets/1, get_accessibility/1, join/3, invite/3, leave/3]).

-record(room, {
    id,
    owner,
    members,
    is_private,
    invited
}).

new(OwnerName, Socket, IsPrivate) ->
    #room{
        id = erlang:unique_integer([positive, monotonic]),
        owner = {OwnerName, Socket},
        members = [{OwnerName, Socket}],
        is_private = IsPrivate,
        invited = [{OwnerName, Socket}]
    }.

get_id(Room) ->
    Room#room.id.

get_owner(Room) ->
    Room#room.owner.

get_owner_name(Room) ->
    {OwnerName, _Socket} = Room#room.owner,
    OwnerName.

get_members_names(Room) ->
    [UserName || {UserName, _} <- Room#room.members].

get_members_sockets(Room) ->
    [Socket || {_, Socket} <- Room#room.members].

get_accessibility(Room) ->
    Room#room.is_private.

join(UserName, Socket, Room) ->
    UpdatedMembers = case lists:member({UserName, Socket}, Room#room.members) of
        true ->
            Room#room.members;
        false ->
            [{UserName, Socket} | Room#room.members]
    end,
    Room#room{members = UpdatedMembers}.

leave(UserName, Socket, Room) ->
    UpdatedMembers = lists:delete({UserName, Socket}, Room#room.members),
    Room#room{members = UpdatedMembers}.

invite(UserName, Socket, Room) ->
    UpdatedInvited = case lists:member({UserName, Socket}, Room#room.invited) of
        true ->
            Room#room.invited;
        false ->
            [{UserName, Socket} | Room#room.invited]
    end,
    Room#room{invited = UpdatedInvited}.