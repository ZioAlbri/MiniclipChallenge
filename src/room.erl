-module(room).
-export([new/2, get_id/1, get_owner/1, get_owner_name/1, get_members_names/1, get_members_sockets/1, get_accessibility/1, join/2, leave/2, invite/2]).

-record(room, {
    id,
    owner,
    members,
    is_private,
    invited
}).

new(Owner, IsPrivate) ->
    #room{
        id = erlang:unique_integer([positive, monotonic]),
        owner = Owner,
        members = [Owner],
        is_private = IsPrivate,
        invited = [Owner]
    }.

get_id(Room) ->
    Room#room.id.

get_owner(Room) ->
    Room#room.owner.

get_owner_name(Room) ->
    {OwnerName, _Socket} = Room#room.owner,
    OwnerName.

get_members_names(Room) ->
    [user:get_name(User) || User <- Room#room.members].

get_members_sockets(Room) ->
    [user:get_socket(User) || User <- Room#room.members].

get_accessibility(Room) ->
    Room#room.is_private.

join(User, Room) ->
    UpdatedMembers = case lists:member(User, Room#room.members) of
        true ->
            Room#room.members;
        false ->
            [User | Room#room.members]
    end,
    Room#room{members = UpdatedMembers}.

leave(User, Room) ->
    UpdatedMembers = lists:delete(User, Room#room.members),
    Room#room{members = UpdatedMembers}.

invite(User, Room) ->
    UpdatedInvited = case lists:member(User, Room#room.invited) of
        true ->
            Room#room.invited;
        false ->
            [User | Room#room.invited]
    end,
    Room#room{invited = UpdatedInvited}.